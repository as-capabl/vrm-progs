{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import RIO hiding (first, second)
import RIO.Vector ((!?))
import qualified RIO.Vector as BV
import qualified Data.Vector.Storable as SV
import qualified RIO.Text as T
import qualified RIO.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable.Mutable as SMV
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import System.Environment (getArgs)
import Data.GlTF
import Graphics.Holz hiding (Vertex, vertexShaderSource, fragmentShaderSource, registerTextures)
import Graphics.GL
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
import qualified Codec.Picture as Jp

die :: MonadIO m => Text -> m a
die s = BS.putStr (encodeUtf8 s) >> exitFailure

data MappedPrim = MappedPrim {
    releasePrim :: IO (),
    drawPrim :: IO ()
}

type MeshMap = IntMap (Vector MappedPrim)

type TxMap = IntMap GLuint

main :: IO ()
main =
  do
    fname : _ <- getArgs
    (gltf, bin) <- liftIO (readGlbRaw fname) >>= \case
        Left err -> die err
        Right x -> return x
    
    scIx <- maybe (die "No default scene\n") return $
        glTFScene gltf

    scene <- maybe (die "Scene index out of range\n") return $
        glTFScenes gltf !? scIx

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

    withHolz $
      do
        w <- openWindow Windowed (Box (V2 0 0) (V2 640 960))
        
        shader <- makeVRMShader
        -- let prog = shaderProg shader
        -- ambient <- getUniform prog "ambient"
        -- planeLight <- getUniform prog "planeLight"
        -- planeDir <- getUniform prog "planeDir"

        (vbs, txs) <- runShaderT shader $ mapToGL gltf bin scene
        vTr <- newIORef initTrans

        forever $ runReaderT `flip` w $ withFrame w $ runShaderT shader $
          do
            windowShouldClose >>= bool (return ()) exitSuccess

            tr <- readIORef vTr
            writeIORef vTr $! rotZX (pi * 0.005) !*! tr

            initView
            liftIO $ with tr $ \p ->
                glUniformMatrix4fv (locationModel shader) 1 1 (castPtr p)
            clearColor grayColor
            drawScene gltf vbs scene
            -- forM_ vbs $ \(tx, env, vb) ->

initView :: MonadHolz m => ShaderT m ()
initView = do
    box@(Box (V2 x0 y0) (V2 x1 y1)) <- getBoundingBox
    setViewport box
    let w = x1 - x0
        h = y1 - y0
    setProjection $ ortho (-w/2) (w/2) (-h/2) (h/2) (-200) 200

initTrans = mkTransformationMat (fromDiag33 400 400 400) (V3 0.0 (-250.0) 0.0) :: M44 Float

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

mapToGL :: forall m. MonadIO m => GlTF -> BinChunk -> Scene -> ShaderT m (MeshMap, TxMap)
mapToGL gltf bin scene =
  do
    refMM <- newIORef IM.empty
    refTM <- newIORef IM.empty
    mapM_ (loadNode refMM refTM) $ sceneNodes scene
    (,) <$> readIORef refMM <*> readIORef refTM
  where
    loadNode refMM refTM nodeId =
      do
        nd <- maybe (die "Out of range in node.") return $
            glTFNodes gltf !? fromIntegral nodeId
        runMaybeT $
          do
            mshId <- MaybeT $ return $ nodeMesh nd
            msh <- MaybeT $ return $ glTFMeshes gltf !? fromIntegral mshId
            lift $ loadMesh refMM refTM mshId msh
        mapM_ (loadNode refMM refTM) $ nodeChildren nd

    loadMesh refMM refTM mshId msh =
      do
        let prims = meshPrimitives msh
        mvbs <- mapM (loadPrim refTM) (V.toList prims)
        modifyIORef' refMM $
            IM.insert mshId (BV.fromListN (V.length prims) $ catMaybes mvbs)

    loadPrim :: IORef TxMap -> MeshPrimitive -> ShaderT m (Maybe MappedPrim)
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
            baseColorTxImg =
              do
                tx <- baseColorTx
                textureSource tx 
            
        BS.putStr $ encodeUtf8 $ T.pack (show material)
        BS.putStr "\n\n"
        BS.putStr $ encodeUtf8 $ T.pack (show baseColorTx)
        BS.putStr "\n\n"
        let attrs = meshPrimitiveAttributes p
            mUv = HM.lookup "TEXCOORD_0" attrs >>= (glTFAccessors gltf !?)
            midcs = meshPrimitiveIndices p >>= (glTFAccessors gltf !?)
        pos <- MaybeT $ return $ HM.lookup "POSITION" attrs >>= (glTFAccessors gltf !?)
        norm <- MaybeT $ return $ HM.lookup "NORMAL" attrs >>= (glTFAccessors gltf !?)
        -- BS.putStr $ T.encodeUtf8 $ T.pack $ show (nodeName nd)
        locDiffuse <- lift $ locationDiffuse <$> ShaderT ask
        registeredTx <- maybe (return Nothing) (loadAndRegisterTexture refTM) baseColorTxImg
        liftIO $
          do
            vao <- overPtr $ glGenVertexArrays 1
            glBindVertexArray vao
            -- VBOs
            let nBuf = 4
            vboM <- SMV.unsafeNew nBuf
            SMV.unsafeWith vboM $ glGenBuffers (fromIntegral nBuf)
            vbo <- SV.unsafeFreeze vboM
            
            glBindBuffer GL_ARRAY_BUFFER $ vbo V.! 0
            glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
            glEnableVertexAttribArray 0
            withAcc gltf pos bin $ \v ->
              do
                glBufferData GL_ARRAY_BUFFER (3 * 4 * fromIntegral (accessorCount pos)) v GL_STATIC_DRAW

            case mUv
              of
                Just uv ->
                  do
                    glBindBuffer GL_ARRAY_BUFFER $ vbo V.! 1
                    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE 0 nullPtr
                    glEnableVertexAttribArray 1
                    withAcc gltf uv bin $ \v ->
                        glBufferData GL_ARRAY_BUFFER (2 * 4 * fromIntegral (accessorCount uv)) v GL_STATIC_DRAW
                Nothing ->
                    return ()
    
            glBindBuffer GL_ARRAY_BUFFER $ vbo V.! 2
            glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE 0 nullPtr
            glEnableVertexAttribArray 2
            withAcc gltf norm bin $ \v ->
                glBufferData GL_ARRAY_BUFFER (3 * 4 * fromIntegral (accessorCount norm)) v GL_STATIC_DRAW

            let releasePrim =
                  do
                    with vao $ glDeleteVertexArrays 1
                    SV.unsafeWith vbo $ glDeleteBuffers (fromIntegral nBuf)

            drawPrim <- case midcs
              of
                Nothing -> undefined
{-                    makeMultiMappedPrim Triangles ElementArray (V.length posP) [
                        vertexV3f 0 posP (ptrOfs pos),
                        vertexV3f 1 normP (ptrOfs norm)
                      ]
-}
                Just idcs ->
                  do
                    let (idxT, eleSiz) = case accessorComponentType idcs
                          of
                            J.Number 5121 -> (GL_UNSIGNED_BYTE, 1)
                            J.Number 5123 -> (GL_UNSIGNED_SHORT, 2)
                            J.Number 5125 -> (GL_UNSIGNED_INT, 4)
                            x -> error $ show x
                        idxCount = fromIntegral $ accessorCount idcs
                    glBindBuffer GL_ELEMENT_ARRAY_BUFFER $ vbo V.! 3
                    withAcc gltf idcs bin $ \v ->
                        glBufferData GL_ELEMENT_ARRAY_BUFFER (idxCount * eleSiz) v GL_STATIC_DRAW
                
                    return $
                      do
                        glBindVertexArray vao
                        setUniform4FV locDiffuse baseColorFactor
                        glBindTexture GL_TEXTURE_2D $ fromMaybe 0 registeredTx
                        glDrawElements GL_TRIANGLES (fromIntegral $ eleSiz * idxCount) idxT nullPtr
            return MappedPrim{..}

    loadAndRegisterTexture :: MonadIO m2 => IORef TxMap -> GlTFid -> m2 (Maybe GLuint)
    loadAndRegisterTexture refTM imgIdx =
      do
        tm <- readIORef refTM
        case IM.lookup imgIdx tm
          of
            Just t -> return (Just t)
            Nothing ->
              do
                let img = glTFImages gltf !? imgIdx
                    bviewIdx = img >>= imageBufferView
                case bviewIdx >>= (glTFBufferViews gltf !?)
                  of
                    Nothing ->
                      do
                        BS.putStr "Warning: no imageBufferView"
                        return Nothing
                    Just bview ->
                      do
                        let ofs = fromMaybe 0 $ bufferViewByteOffset bview
                            len = bufferViewByteLength bview
                        eth <- liftIO $ withForeignPtr bin $ \p ->
                          do
                            b <- BS.unsafePackCStringLen (p `plusPtr` ofs, len)
                            return $! Jp.decodeImage b
                        case eth
                          of
                            Left s -> trace (T.pack s) $ return Nothing
                            Right di ->
                              do
                                let rgba8@(Jp.Image w h _) = Jp.convertRGBA8 di
                                idx <- registerTextures (V2 w h) [(V2 0 0, rgba8)]
                                return (Just idx)
                        


                    
        
        
        
                    
listToV4 :: Vector J.Value -> Maybe (V4 Float)
listToV4 v = V4 <$> toF (v !? 0) <*> toF (v !? 1) <*> toF (v !? 2) <*> toF (v !? 3)
  where
    toF (Just (J.fromJSON -> J.Success n)) = Just n
    toF _ = Nothing

            
withAcc :: forall b m. (MonadIO m, MonadUnliftIO m) => GlTF -> Accessor -> BinChunk -> (Ptr () -> m b) -> m b
withAcc gltf acc bin f =
  do
    bview <- maybe (die "BufferView") return $
      do
        mx <- accessorBufferView acc
        glTFBufferViews gltf !? mx
    let
        ofs = fromMaybe 0 (bufferViewByteOffset bview) 
    withRunInIO $ \run -> withForeignPtr bin $ \p ->
        run $ f (p `plusPtr` ofs)

ptrOfs acc = nullPtr `plusPtr` fromMaybe 0 (accessorByteOffset acc)

setUniform3FV :: MonadIO m => GLint -> V3 Float -> m ()
setUniform3FV loc col = liftIO $ with col $ glUniform3fv loc 1 . castPtr

setUniform4FV :: MonadIO m => GLint -> V4 Float -> m ()
setUniform4FV loc col = liftIO $ with col $ glUniform4fv loc 1 . castPtr


drawScene :: MonadHolz m => GlTF -> MeshMap -> Scene -> ShaderT m ()
drawScene gltf mm scene = mapM_ drawNode $ sceneNodes scene
  where
    drawNode nodeId =
      do
        nd <- maybe (die "Node out of range") return $ glTFNodes gltf !? fromIntegral nodeId
        runMaybeT $
          do
            mshId <- MaybeT $ return $ nodeMesh nd
            mshVAOs <- MaybeT $ return $ IM.lookup mshId mm
            drawMesh mshVAOs
        mapM_ drawNode $ nodeChildren nd

    drawMesh mshVAOs = V.forM_ mshVAOs $ \vao ->
        liftIO $ drawPrim vao

makeVRMShader :: IO Shader
makeVRMShader = do
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
    locationDiffuse <- getUniform shaderProg "baseColorFactor"
    locationSpecular <- getUniform shaderProg "specular"

    with (V4 1 1 1 1 :: V4 Float) $ \ptr -> do
        glUniform4fv locationDiffuse 1 (castPtr ptr)
        glUniform4fv locationSpecular 1 (castPtr ptr)

    return Shader{..}


vertexShaderSource :: String
vertexShaderSource = "#version 330\n\
    \uniform mat4 projection; \
    \uniform mat4 model; \
    \in vec3 in_Position; \
    \in vec2 in_UV; \
    \in vec3 in_Normal; \
    \out vec2 texUV; \
    \out vec3 normal; \
    \out vec4 viewPos; \
    \out vec4 color; \
    \void main(void) { \
    \  viewPos = model * vec4(in_Position, 1.0); \
    \  gl_Position = projection * viewPos; \
    \  texUV = in_UV; \
    \  normal = in_Normal;\
    \  color = vec4(1.0, 1.0, 1.0, 1.0);\
    \}"

fragmentShaderSource :: String
fragmentShaderSource = "#version 330\n\
    \out vec4 fragColor; \
    \in vec2 texUV; \
    \in vec3 normal; \
    \in vec4 viewPos; \
    \in vec4 color; \
    \uniform sampler2D tex; \
    \uniform vec4 baseColorFactor; \
    \uniform vec3 specular; \
    \void main(void){ \
    \  fragColor = texture(tex, texUV) * color * baseColorFactor; \
    \}"

registerTextures :: MonadIO m => V2 Int -> [(V2 Int, Jp.Image Jp.PixelRGBA8)] -> m GLuint
registerTextures (V2 sw sh) imgs = liftIO $ do
    tex <- overPtr (glGenTextures 1)
    glBindTexture GL_TEXTURE_2D tex
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
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

    forM_ imgs $ \(V2 x y, Jp.Image w h vec) -> SV.unsafeWith vec
        $ glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE
        . castPtr

    glGenerateMipmap GL_TEXTURE_2D

    return tex
