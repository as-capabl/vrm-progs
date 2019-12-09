{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module
    Graphics.GlTF
where

import RIO hiding (first, second)
import RIO.Vector ((!?))
import qualified Data.ByteString.Unsafe as BS
import qualified RIO.Vector as BV
import qualified Data.Vector.Storable as SV
import qualified RIO.Text as T
import qualified RIO.Vector as V
import qualified RIO.Vector.Unsafe as V
import qualified RIO.Vector.Partial as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Storable.Mutable as SMV
import qualified Data.IntMap as IM
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Data.GlTF
import Graphics.Holz.System hiding (registerTextures, Texture)
import Graphics.GL
import Linear hiding (trace)
import qualified Data.HashMap.Strict as HM
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils (with)
import qualified Codec.Picture as Jp

import Graphics.GlTF.Type
import Graphics.GlTF.Shader
import Graphics.GlTF.Util

mapToGL ::
    forall m env.
    (MonadIO m, HasLogFunc env, MonadReader env m, HasCallStack) =>
    GlTF -> BinChunk -> Scene -> MRShaderT m (MeshMap, TxMap)
mapToGL gltf bin scene =
  do
    refMM <- liftIO $ MV.replicate (V.length $ glTFNodes gltf) (V.empty)
    refTM <- newIORef IM.empty
    mapM_ (loadNode refMM refTM) $ sceneNodes scene
    (,) <$> liftIO (V.unsafeFreeze refMM) <*> readIORef refTM
  where
    loadNode refMM refTM nodeId = warnOnException $
      do
        nd <- maybe (throwError "Out of range in node.") return $
            glTFNodes gltf !? fromIntegral nodeId
        _ <- runMaybeT $
          do
            mshId <- MaybeT $ return $ nodeMesh nd
            msh <- MaybeT $ return $ glTFMeshes gltf !? fromIntegral mshId
            mvbs <- lift . lift $ loadMesh refTM msh
            liftIO $ MV.write refMM nodeId (BV.fromList $ catMaybes mvbs)
        mapM_ (lift . loadNode refMM refTM) $ nodeChildren nd

    loadMesh refTM msh =
      do
        let prims = meshPrimitives msh
        mapM (loadPrim refTM) (V.toList prims)

    loadPrim :: IORef TxMap -> MeshPrimitive -> MRShaderT m (Maybe MappedPrim)
    loadPrim refTM p = runMaybeT $
      do
        let material = meshPrimitiveMaterial p >>= (glTFMaterials gltf !?)
            materialPbr = material >>= materialPbrMetallicRoughness
            baseColorFactor = fromMaybe (V4 1.0 1.0 1.0 1.0) $
              do
                m <- materialPbr
                listToV4 $ materialPbrMetallicRoughnessBaseColorFactor m
            baseColorTx =
              do
                m <- materialPbr
                txInfo <- materialPbrMetallicRoughnessBaseColorTexture m
                glTFTextures gltf !? textureInfoIndex txInfo
            metallic = fromMaybe 0 $ materialPbr >>= materialPbrMetallicRoughnessMetallicFactor
            roughness = fromMaybe 0 $ materialPbr >>= materialPbrMetallicRoughnessRoughnessFactor

        -- logInfo (displayShow material)
        let attrs = meshPrimitiveAttributes p
            mUv = HM.lookup "TEXCOORD_0" attrs >>= (glTFAccessors gltf !?)
            midcs = meshPrimitiveIndices p >>= (glTFAccessors gltf !?)
        -- logInfo (displayShow attrs)
        pos <- MaybeT $ return $ HM.lookup "POSITION" attrs >>= (glTFAccessors gltf !?)
        nm <- MaybeT $ return $ HM.lookup "NORMAL" attrs >>= (glTFAccessors gltf !?)
        -- BS.putStr $ T.encodeUtf8 $ T.pack $ show (nodeName nd)
        locDiffuse <- lift $ locationBaseColorFactor <$> MRShaderT ask
        locLamFactor <- lift $ locationLamFactor <$> MRShaderT ask
        locNlamFactor <- lift $ locationNlamFactor <$> MRShaderT ask
        registeredTx <- maybe (return Nothing) (lift . loadAndRegisterTexture refTM) baseColorTx

        let drawPrim_elem idcs vao =
              do
                let idxT = accessorComponentType idcs
                    eleSiz = sizeOfGLType idxT
                    idxCount = fromIntegral $ accessorCount idcs
                glBindVertexArray vao
                setUniform4FV locDiffuse baseColorFactor
                glUniform1f locLamFactor $ metallic
                glUniform1f locNlamFactor $ roughness
                glBindTexture GL_TEXTURE_2D $ fromMaybe 0 registeredTx
                glDrawElements GL_TRIANGLES (eleSiz * idxCount) idxT nullPtr

        let (idcBuf, drawPrim_) = case midcs
              of
                Nothing -> undefined
                Just idcs ->
                    ([bufferDataByAccessor gltf bin ElementArrayBuffer idcs], drawPrim_elem idcs)

        let vboSrc = [bufferDataByAccessor gltf bin (ArrayBuffer 0) pos]
                ++ (case mUv
                  of
                    Just uv -> [bufferDataByAccessor gltf bin (ArrayBuffer 1) uv]
                    Nothing -> [])
                ++ [bufferDataByAccessor gltf bin (ArrayBuffer 2) nm]
                ++ idcBuf

        let nBuf = length vboSrc
        (vao, vbo) <- liftIO $
          do
            vao <- overPtr $ glGenVertexArrays 1
            glBindVertexArray vao
            vboM <- MV.unsafeNew nBuf
            SMV.unsafeWith vboM $ glGenBuffers (fromIntegral nBuf)
            vbo <- SV.unsafeFreeze vboM
            forM_ (zip vboSrc [0..]) $ \(f, i) -> f vbo i
            return (vao, vbo)

        let drawPrim =
                drawPrim_ vao

            releasePrim =
              do
                with vao $ glDeleteVertexArrays 1
                SV.unsafeWith vbo $ glDeleteBuffers (fromIntegral nBuf)

        return MappedPrim{..}

    loadAndRegisterTexture :: IORef TxMap -> Texture -> MRShaderT m (Maybe GLuint)
    loadAndRegisterTexture refTM tx = warnOnException $
      do
        imgIdx <- maybe (throwError "Image") return $ textureSource tx
        tm <- readIORef refTM
        case IM.lookup imgIdx tm
          of
            Just t -> return t
            Nothing ->
              do
                -- Get bufferView
                let img = glTFImages gltf !? imgIdx
                    bviewIdx = img >>= imageBufferView
                bview <-
                    maybe (throwError "No imageBufferView") return $
                        bviewIdx >>= (glTFBufferViews gltf !?)
                let ofs = fromMaybe 0 $ bufferViewByteOffset bview
                    len = bufferViewByteLength bview
                -- Load image
                eth <- liftIO $ withForeignPtr bin $ \p ->
                  do
                    b <- BS.unsafePackCStringLen (p `plusPtr` ofs, len)
                    return $! Jp.decodeImage b
                di <- case eth
                  of
                    Left s -> throwError $ "Texture load error: " <> display (T.pack s)
                    Right x -> return x
                let rgba8@(Jp.Image w h _) = Jp.convertRGBA8 di
                -- Load sampler
                let empSmp = Sampler Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                    smp = fromMaybe empSmp $
                      do
                        smpIdx <- textureSampler tx
                        glTFSamplers gltf !? smpIdx
                logInfo $ displayShow smp
                idx <- registerTextures smp (V2 w h) [(V2 0 0, rgba8)]
                return idx



data TargetBuffer = ArrayBuffer GLuint | ElementArrayBuffer

bufferDataByAccessor ::
    (MonadIO m, MonadUnliftIO m, MonadThrow m) =>
    GlTF -> BinChunk -> TargetBuffer -> Accessor -> SV.Vector GLuint -> Int -> m ()
bufferDataByAccessor gltf bin tgt acc@Accessor{..} vbo i =
  do
    glBindBuffer bufKind $ vbo V.! i
    bufSet
    withAcc gltf acc bin $ \v ->
        glBufferData bufKind (fromIntegral $ fromIntegral nEle * eleSiz * accessorCount) v GL_STATIC_DRAW
  where
    nEle = case accessorType
      of
        AccScalar -> 1
        AccVec2 -> 2
        AccVec3 -> 3
        AccVec4 -> 4
        _ -> error "bufferDataByAccessor"
    eleSiz = sizeOfGLType accessorComponentType
    (bufKind, bufSet) = case tgt
      of
        ArrayBuffer nAttr -> (GL_ARRAY_BUFFER, bufSetArray nAttr)
        ElementArrayBuffer -> (GL_ELEMENT_ARRAY_BUFFER, return ())
    bufSetArray nAttr =
      do
        let nm = case accessorNormalized
                   of
                     Just True -> GL_TRUE
                     _ -> GL_FALSE
        glVertexAttribPointer nAttr nEle accessorComponentType nm 0 nullPtr
        glEnableVertexAttribArray nAttr


-- ptrOfs acc = nullPtr `plusPtr` fromMaybe 0 (accessorByteOffset acc)


drawScene ::
    MonadIO m =>
    GlTF -> MeshMap -> Scene -> Maybe NodeAniState -> Float -> MRShaderT m ()
drawScene gltf mm scene aniState delta = mapM_ drawNode $ sceneNodes scene
  where
    drawNode nodeId =
      do
        nd <- maybe (error "Node out of range") return $
          do
            glTFNodes gltf !? fromIntegral nodeId
        _ <- runMaybeT $
          do
            prims <- MaybeT $ return $ mm !? nodeId
            V.forM_ prims $ \prim ->
                liftIO $ drawPrim prim
        mapM_ drawNode $ nodeChildren nd

