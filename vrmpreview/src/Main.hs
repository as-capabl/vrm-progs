{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import RIO hiding (first, second)
import RIO.Vector ((!?))
import qualified RIO.Vector as BV
import qualified Data.Vector.Storable as SV
import qualified RIO.Text as T
import qualified RIO.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified RIO.Vector as V
import qualified RIO.Vector.Unsafe as V
import qualified RIO.Vector.Partial as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Storable.Mutable as SMV
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Control.Monad.Except
import System.Environment (getArgs)
import Data.GlTF
import Data.BoundingBox (union)
import Graphics.Holz.System hiding (registerTextures, Texture)
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
import Linear hiding (trace)
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as J
import qualified Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (with)
import Foreign.C.String
import Foreign.Storable
import Foreign.Storable.Generic
import qualified Codec.Picture as Jp
import Text.RawString.QQ (r)

data MRShader = MRShader {
    shaderProg :: GLuint,
    locationModel :: GLint,
    locationProjection :: GLint,
    locationPlaneDir :: GLint,
    locationSight :: GLint,
    locationBaseColorFactor :: GLint,
    locationMetallic :: GLint,
    locationLamFactor :: GLint,
    locationNlamFactor :: GLint
  }

newtype MRShaderT m a = MRShaderT {
    unMRShaderT :: ReaderT MRShader m a
  }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance MonadReader r m => MonadReader r (MRShaderT m)
  where
    ask = lift ask
    local f (MRShaderT (ReaderT fmx)) = MRShaderT $ ReaderT $ \vrm -> local f (fmx vrm)

runMRShaderT :: MonadIO m => MRShader -> MRShaderT m a -> m a
runMRShaderT shader action =
  do
    glUseProgram $ shaderProg shader
    runReaderT (unMRShaderT action) shader

askMRShader :: Monad m => (MRShader -> a) -> MRShaderT m a
askMRShader f = MRShaderT $ f <$> ask

die :: MonadIO m => Text -> m a
die s = BS.putStr (encodeUtf8 s) >> exitFailure

data MappedPrim = MappedPrim {
    releasePrim :: IO (),
    drawPrim :: IO ()
  }

type MeshMap = Vector (Vector MappedPrim)

type TxMap = IntMap GLuint


-- data DrawState =

canvasSize :: V2 Float
canvasSize = V2 640 960

main :: IO ()
main =
  do
    fname : _ <- getArgs
    (gltf, bin) <- liftIO (readGlb fname) >>= \case
        Left err -> die err
        Right x -> return x

    scIx <- maybe (die "No default scene\n") return $
        glTFScene gltf

    scene <- maybe (die "Scene index out of range\n") return $
        glTFScenes gltf !? scIx

    let ani = glTFAnimations gltf !? 0
    {-
    let accs = glTFAccessors gltf
    forM_ accs $ \acc ->
      do
        case J.fromJSON $ accessorType acc
          of
            J.Success s -> BS.putStr $ encodeUtf8 s
            J.Error err -> BS.putStr $ "Error:" <> fromString err
        BS.putStr ","
        case J.fromJSON $ accessorComponentType acc
            of
              J.Success i -> BS.putStr $ dispEType i
              J.Error err -> BS.putStr $ "Error:" <> fromString err
        BS.putStr "\n"
    -}

    let bbox = calcBBox gltf scene

    logOpt <- logOptionsHandle stderr False
    withHolz $ withLogFunc logOpt $ \logFunc -> runRIO logFunc $
      do
        w <- liftIO $ openWindow Windowed (Box (V2 0 0) canvasSize)
        logInfo $ displayShow bbox

        shader <- makeVRMShader
        -- let prog = shaderProg shader
        -- ambient <- getUniform prog "ambient"
        -- planeLight <- getUniform prog "planeLight"
        -- planeDir <- getUniform prog "planeDir"


        (vbs, txs) <- runMRShaderT shader $ mapToGL gltf bin scene
        aniState <- mapM (initAniState gltf bin) ani
        vTr <- newIORef (initTrans bbox)
        vT <-
          do
            t0 <- liftIO $ fromMaybe 0 <$> GLFW.getTime
            newIORef t0

        forever $ runReaderT `flip` w $ withFrame w $ runMRShaderT shader $
          do
            windowShouldClose >>= bool (return ()) exitSuccess

            delta <-
              do
                t <- readIORef vT
                t' <- liftIO $ fromMaybe 0 <$> GLFW.getTime
                writeIORef vT t'
                return $ realToFrac (t' - t)

            tr <- readIORef vTr
            writeIORef vTr $! rotZX (delta * pi * 0.5) !*! tr

            initView
            liftIO $ with tr $ \p ->
                glUniformMatrix4fv (locationModel shader) 1 1 (castPtr p)
            liftIO $ with (V3 0.577 0.577 0.577 :: V3 Float) $ \p ->
                glUniform3fv (locationPlaneDir shader) 1 (castPtr p)
            clearColor grayColor
            drawScene gltf vbs scene aniState delta
            -- forM_ vbs $ \(tx, env, vb) ->

initView :: MonadHolz m => MRShaderT m ()
initView =
  do
    box@(Box (V2 x0 y0) (V2 x1 y1)) <- getBoundingBox
    setViewport box
    let w = x1 - x0
        h = y1 - y0
        proj = ortho (-w/2) (w/2) (-h/2) (h/2) (-w/2) (w/2)
        sight = V3 0 0 (-1) :: V3 Float
    locProjection <- askMRShader locationProjection
    liftIO $ with proj $ \p ->
        glUniformMatrix4fv locProjection 1 1 (castPtr p)
    locSight <- askMRShader locationSight
    liftIO $ with sight $ \p ->
        glUniform3fv locSight 1 (castPtr p)


initTrans :: Box V3 Float -> M44 Float
initTrans bbox = mkTransformationMat (fromDiag33 r r r) (V3 0.0 vOff 0.0)
  where
    Box (V3 lModel bModel _) (V3 rModel tModel _) = bbox
    wModel = rModel - lModel
    hModel = tModel - bModel
    V2 wView hView = canvasSize
    wRatio = wView / (wModel + 0.0001)
    hRatio = hView / (hModel + 0.0001)
    r = min wRatio hRatio
    vOff = - (r * (tModel - bModel) / 2)

fromDiag33 :: Num a => a -> a -> a -> M33 a
fromDiag33 x y z = V3 (V3 x 0 0) (V3 0 y 0) (V3 0 0 z)

fromDiag44 :: Num a => a -> a -> a -> a -> M44 a
fromDiag44 x y z w = V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 z 0) (V4 0 0 0 w)


rotYZ :: Floating a => a -> M44 a
rotYZ th = mkTransformationMat rotM trV
  where
    rotM = V3 (V3 1 0 0) (V3 0 (cos th) (sin th)) (V3 0 (- sin th) (cos th))
    trV = V3 0 0 0

rotZX :: Floating a => a -> M44 a
rotZX th = mkTransformationMat rotM trV
  where
    rotM = V3 (V3 (cos th) 0 (- sin th)) (V3 0 1 0) (V3 (sin th) 0 (cos th))
    trV = V3 0 0 0


grayColor = V4 0.7 0.7 0.7 1.0

blueColor = V4 0 0 1.0 (0.5)

-- data MatVert = MatVert Material (V.Vector Pmx.Vertex)

dispEType :: (IsString s) => Int -> s
dispEType 5120 = "BYTE"
dispEType 5121 = "UNSIGNED_BYTE"
dispEType 5122 = "SHORT"
dispEType 5123 = "UNSIGNED_SHORT"
dispEType 5125 = "UNSIGNED_INT"
dispEType 5126 = "FLOAT"
dispEType i = "Unknown"

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
    loadNode refMM refTM nodeId =
      do
        nd <- maybe (die "Out of range in node.") return $
            glTFNodes gltf !? fromIntegral nodeId
        runMaybeT $
          do
            mshId <- MaybeT $ return $ nodeMesh nd
            msh <- MaybeT $ return $ glTFMeshes gltf !? fromIntegral mshId
            mvbs <- lift $ loadMesh refTM mshId msh
            liftIO $ MV.write refMM nodeId (BV.fromList $ catMaybes mvbs)
        mapM_ (loadNode refMM refTM) $ nodeChildren nd

    loadMesh refTM mshId msh =
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
        norm <- MaybeT $ return $ HM.lookup "NORMAL" attrs >>= (glTFAccessors gltf !?)
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
                glDrawElements GL_TRIANGLES (fromIntegral $ eleSiz * idxCount) idxT nullPtr

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
                ++ [bufferDataByAccessor gltf bin (ArrayBuffer 2) norm]
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


sizeOfGLType GL_UNSIGNED_BYTE = 1
sizeOfGLType GL_UNSIGNED_SHORT = 2
sizeOfGLType GL_UNSIGNED_INT = 4
sizeOfGLType GL_FLOAT = 4
sizeOfGLType x = error $ "sizeOfGLType " ++ show x

data TargetBuffer = ArrayBuffer GLuint | ElementArrayBuffer

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



warnOnException ::
    (MonadIO m, HasLogFunc env, MonadReader env m, HasCallStack) =>
    ExceptT Utf8Builder m a -> m (Maybe a)
warnOnException mx = runExceptT mx >>= \case
    Left s -> logWarn s >> return Nothing
    Right x -> return $ Just x

listToV3 :: Vector Float -> Maybe (V3 Float)
listToV3 v = V3 <$> (v !? 0) <*> (v !? 1) <*> (v !? 2)

listToV4 :: Vector Float -> Maybe (V4 Float)
listToV4 v = V4 <$> (v !? 0) <*> (v !? 1) <*> (v !? 2) <*> (v !? 3)

ptrOfs acc = nullPtr `plusPtr` fromMaybe 0 (accessorByteOffset acc)

setUniform3FV :: MonadIO m => GLint -> V3 Float -> m ()
setUniform3FV loc col = liftIO $ with col $ glUniform3fv loc 1 . castPtr

setUniform4FV :: MonadIO m => GLint -> V4 Float -> m ()
setUniform4FV loc col = liftIO $ with col $ glUniform4fv loc 1 . castPtr

newtype BoxUnion f a = BoxUnion { unBoxUnion :: Box f a }

instance
    (Applicative f, Ord a) => Semigroup (BoxUnion f a)
  where
    BoxUnion x <> BoxUnion y = let box@(Box !_ !_) = union x y in BoxUnion box

calcBBox :: GlTF -> Scene -> Box V3 Float
calcBBox gltf scene =
    unBoxUnion . fromMaybe empBox $ foldMap bboxNode (sceneNodes scene)
  where
    empBox = BoxUnion (Box (V3 (-1) (-1) (-1)) (V3 1 1 1))
    bboxNode ndId =
      do
        nd <- glTFNodes gltf !? fromIntegral ndId
        let self =
              do
                mshId <- nodeMesh nd
                msh <- glTFMeshes gltf !? fromIntegral mshId
                foldMap bboxPrim (meshPrimitives msh)
        self <> foldMap bboxNode (nodeChildren nd)
    bboxPrim pm =
      do
        accId <- HM.lookup "POSITION" $ meshPrimitiveAttributes pm
        acc <- glTFAccessors gltf !? fromIntegral accId
        minimum <- listToV3 $ accessorMin acc
        maximum <- listToV3 $ accessorMax acc
        return $ BoxUnion (Box minimum maximum)


drawScene :: MonadHolz m => GlTF -> MeshMap -> Scene -> Maybe NodeAniState -> Float -> MRShaderT m ()
drawScene gltf mm scene aniState delta = mapM_ drawNode $ sceneNodes scene
  where
    drawNode nodeId =
      do
        nd <- maybe (die "Node out of range") return $
          do
            glTFNodes gltf !? fromIntegral nodeId
        runMaybeT $
          do
            prims <- MaybeT $ return $ mm !? nodeId
            V.forM_ prims $ \prim ->
                liftIO $ drawPrim prim
        mapM_ drawNode $ nodeChildren nd

--
-- Animation
--
{-
data LiveAnimationChannel a = LiveAnimationChannel
  {
    lacValid :: Bool,
    lacInput :: Ptr Float,
    lacOutput :: Ptr (),
    lacOutputConv :: Ptr () -> IO a,
    lacCurrentTime :: Float,
    lacCurrentIndex :: Int
  }

data LiveAnimation = LiveAnimation
  {
    laTranslation :: LiveAnimationChannel (V3 Float),
    laTranslation :: LiveAnimationChannel (V4 Float),
    laScale :: LiveAnimationChannel (V3 Float)
  }
-}

data AniChanState = AniChanState
  {
    acsCurrentTime :: Float,
    acsCurrentIndex :: Int
  }
  deriving (Eq, Show, Generic)

instance GStorable AniChanState

data AniState = AniState
  {
    asSpatialAni :: Vector (Maybe (AnimationChannel, AnimationSampler, Accessor, Accessor)),
    asSpatialState :: SMV.IOVector AniChanState
  }

type NodeAniState = Vector AniState

initAniState ::
    (MonadIO m, MonadUnliftIO m, HasLogFunc env, MonadReader env m) =>
    GlTF -> BinChunk -> Animation -> m NodeAniState
initAniState gltf bin Animation{..} =
  do
    mv <- withUnliftIO $  \uio ->
        MV.replicateM (V.length $ glTFNodes gltf) (unliftIO uio newAniState)
    liftIO $ V.unsafeFreeze mv
  where
    newAniState =
      do
        cs <- liftIO $ MV.replicate 3 (AniChanState 0 0)
        return $ AniState (V.replicate 3 Nothing) cs

data Interpolator a = Interpolator
  {
    ipSize :: Int,
    ipRead :: Ptr () -> Int -> IO a,
    ipDefault :: a,
    ipLinear :: (Float, a) -> (Float, a) -> Float -> a
  }

linearInterpolation :: (Num a, Fractional a) => (Float, a) -> (Float, a) -> Float -> a
linearInterpolation (t1, x1) (t2, x2) t
    | delta > dmin = x1 + (x2 - x1) * realToFrac ((t - t1) / delta)
    | otherwise = (x1 + x2) / 2
  where
    delta = t2 - t1
    dmin = 0.00000001

ipScale :: Interpolator (V3 Float)
ipScale = Interpolator {..}
  where
    ipSize = sizeOf (undefined :: V3 Float)
    ipRead p i = peekElemOff (castPtr p) i
    ipDefault = V3 1 1 1
    ipLinear = linearInterpolation

ipRotate :: Interpolator (Quaternion Float)
ipRotate = Interpolator {..}
  where
    ipSize = sizeOf (undefined :: Float) * 4
    ipRead p0 i =
      do
        let p = castPtr @Float p
        x <- peekElemOff p (4 * i)
        y <- peekElemOff p (4 * i + 1)
        z <- peekElemOff p (4 * i + 2)
        w <- peekElemOff p (4 * i + 3)
        return $ Quaternion w (V3 x y z)
    ipDefault = 1
    ipLinear = linearInterpolation

ipTrans :: Interpolator (V3 Float)
ipTrans = Interpolator {..}
  where
    ipSize = sizeOf (undefined :: V3 Float)
    ipRead p i = peekElemOff (castPtr p) i
    ipDefault = V3 0 0 0
    ipLinear = linearInterpolation

tickAniState :: MonadIO m => GlTF -> BinChunk -> AniState -> Float -> m (M44 Float)
tickAniState gltf bin AniState{..} delta = liftIO $
  do
    V3 sx sy sz <- doTick 0 ipScale
    let sMat = V3 (V3 sx 0 0) (V3 0 sy 0) (V3 0 0 sz)
    r <- doTick 1 ipRotate
    t <- doTick 2 ipTrans
    return $ mkTransformationMat (fromQuaternion r !*! sMat) t
  where
    doTick :: Int -> Interpolator a -> IO a
    doTick i ip = doTickImpl (asSpatialAni V.! i) i ip

    doTickImpl ::
        Maybe (AnimationChannel, AnimationSampler, Accessor, Accessor) -> Int -> Interpolator a -> IO a
    doTickImpl (Just (chan, smp, inAcc, outAcc)) i ip =
      do
        AniChanState {..} <- MV.read asSpatialState i
        let Just tMax = accessorMax inAcc !? 0
            count = accessorCount inAcc
            t'0 = acsCurrentTime + delta
            -- TODO: Currently fixed to repeat. Supprot truncation.
            (t', idx0) = if t'0 < tMax then (t'0, acsCurrentIndex) else (0, 0)
        (i1, i2) <- withAcc gltf inAcc bin $ \p ->
            seekIdx (castPtr p) count idx0 t'
        (t1, t2) <- withAcc gltf inAcc bin $ \p ->
            (,) <$> peekElemOff (castPtr p) i1 <*> peekElemOff (castPtr p) i2
        (x1, x2) <- withAcc gltf outAcc bin $ \p ->
            (,) <$> ipRead ip p i1 <*> ipRead ip p i2
        MV.write asSpatialState i (AniChanState t' i1)
        return $ ipLinear ip (t1, x1) (t2, x2) t'
      where
        seekIdx p count idx t
            | idx == 0 =
              do
                t0 <- peekElemOff p 0
                if t < t0 then return (0, 0) else seekIdx1 p count idx t
            | otherwise = seekIdx1 p count idx t
        seekIdx1 p count idx t
            | idx + 1 < count =
              do
                let !idx' = idx + 1
                t' <- peekElemOff p idx'
                if t < t' then return (idx, idx') else seekIdx1 p count idx' t
            | otherwise = return (idx, idx)

    doTickImpl Nothing _ ip =
        return $ ipDefault ip

--
-- Shader
--
makeVRMShader :: MonadIO m => m MRShader
makeVRMShader = liftIO $
  do
    vertexShader <- glCreateShader GL_VERTEX_SHADER
    fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
    compileShader vertexShaderSource vertexShader
    compileShader fragmentShaderSource fragmentShader

    shaderProg <- glCreateProgram
    glAttachShader shaderProg vertexShader
    glAttachShader shaderProg fragmentShader

    withCString "in_Position" $ glBindAttribLocation shaderProg 0
    withCString "in_UV" $ glBindAttribLocation shaderProg 1
    withCString "in_Normal" $ glBindAttribLocation shaderProg 2

    glLinkProgram shaderProg
    glUseProgram shaderProg

    linked <- overPtr (glGetProgramiv shaderProg GL_LINK_STATUS)
    when (linked == GL_FALSE) $ do
        maxLength <- overPtr (glGetProgramiv shaderProg GL_INFO_LOG_LENGTH)
        allocaArray (fromIntegral maxLength) $ \ptr -> do
            glGetProgramInfoLog shaderProg maxLength nullPtr ptr
            BS.packCString ptr >>= BS.putStr

    locationModel <- getUniform shaderProg "model"
    locationProjection <- getUniform shaderProg "projection"
    locationSight <- getUniform shaderProg "sight"
    locationBaseColorFactor <- getUniform shaderProg "baseColorFactor"
    locationPlaneDir <- getUniform shaderProg "planeDir"
    locationLamFactor <- getUniform shaderProg "lamFactor"
    locationNlamFactor <- getUniform shaderProg "nlamFactor"
    locationMetallic <- getUniform shaderProg "metallic"

    with (V4 1 1 1 1 :: V4 Float) $ \ptr -> do
        glUniform4fv locationBaseColorFactor 1 (castPtr ptr)

    with (V3 1 1 1 :: V3 Float) $ \ptr -> do
        glUniform4fv locationPlaneDir 1 (castPtr ptr)

    return MRShader{..}


vertexShaderSource :: String
vertexShaderSource = [r|
    #version 330
    uniform mat4 projection;
    uniform mat4 model;
    in vec3 in_Position;
    in vec2 in_UV;
    in vec3 in_Normal;
    out vec2 texUV;
    out vec3 normal;
    out vec4 viewPos;
    out vec4 color;
    void main(void) {
        viewPos = model * vec4(in_Position, 1.0);
        gl_Position = projection * viewPos;
        texUV = in_UV;
        normal = mat3(model) * in_Normal;
        color = vec4(1.0, 1.0, 1.0, 1.0);
    }
|]

fragmentShaderSource :: String
fragmentShaderSource = [r|
    #version 330
    #define PI 3.1415926538
    #define dielectricSpecular vec4(0.04, 0.04, 0.04, 1)
    #define black vec4(0, 0, 0, 1)
    out vec4 fragColor;
    in vec2 texUV;
    in vec3 normal;
    in vec4 viewPos;
    in vec4 color;
    in float metallic;
    uniform sampler2D tex;
    uniform vec4 baseColorFactor;
    uniform vec3 planeDir;
    uniform vec3 sight;
    uniform float lamFactor;
    uniform float nlamFactor;
    void main(void){
        vec3 nnorm = normalize(normal);
        vec3 h = normalize(sight + planeDir);

        vec4 baseColor = texture(tex, texUV) * color * baseColorFactor;
        vec4 dfColor = mix(baseColor * (1 - dielectricSpecular[0]), black, lamFactor);
        vec4 spColor = mix(dielectricSpecular, baseColor, lamFactor);

        float lamValue = dot(nnorm, planeDir);
        float spValue = dot(nnorm, h);

        vec4 fresnel = spColor + (vec4(1,1,1,1) - spColor) * pow(1.0 - dot(sight, h), 5);
        fragColor =
            spValue * fresnel
            + lamValue * ((vec4(1,1,1,1) - fresnel) * dfColor) * 0.5
            + 0.5 * baseColor
            ;
        fragColor[3] = 1.0;
    }
|]

registerTextures :: MonadIO m => Sampler -> V2 Int -> [(V2 Int, Jp.Image Jp.PixelRGBA8)] -> m GLuint
registerTextures Sampler{..} (V2 sw sh) imgs = liftIO $ do
    tex <- overPtr (glGenTextures 1)
    glBindTexture GL_TEXTURE_2D tex
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER $ fromMaybe GL_LINEAR samplerMinFilter
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER $ fromMaybe GL_LINEAR samplerMagFilter
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S $ fromMaybe GL_REPEAT samplerWrapS
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T $ fromMaybe GL_REPEAT samplerWrapT
    glPixelStorei GL_UNPACK_ALIGNMENT 4
    glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
    glPixelStorei GL_UNPACK_LSB_FIRST 0
    glPixelStorei GL_UNPACK_ROW_LENGTH 0
    glPixelStorei GL_UNPACK_SKIP_IMAGES 0
    glPixelStorei GL_UNPACK_SKIP_PIXELS 0
    glPixelStorei GL_UNPACK_SKIP_ROWS 0
    glPixelStorei GL_UNPACK_SWAP_BYTES 0
    let level = floor $ logBase (2 :: Float) $ fromIntegral (max sw sh)

    glTexStorage2D GL_TEXTURE_2D level GL_SRGB8_ALPHA8 (fromIntegral sw) (fromIntegral sh)

    forM_ imgs $ \(V2 x y, Jp.Image w h vec) -> SV.unsafeWith vec $
        glTexSubImage2D
            GL_TEXTURE_2D 0
            (fromIntegral x)
            (fromIntegral y)
            (fromIntegral w)
            (fromIntegral h)
            GL_RGBA
            GL_UNSIGNED_BYTE
        . castPtr

    glGenerateMipmap GL_TEXTURE_2D

    return tex
