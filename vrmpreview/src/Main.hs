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
{-# LANGUAGE TypeFamilies #-}

module Main where

import RIO hiding (first, second)
import RIO.Vector ((!?))
import qualified RIO.Vector as V
import qualified RIO.ByteString as BS
import Control.Monad.Trans.Maybe
import System.Environment (getArgs)
import Data.GlTF
import Data.BoundingBox (union)
import Graphics.Holz.System
import Graphics.Holz.Shader
import qualified Graphics.Holz.Input as HIn
import qualified Graphics.UI.GLFW as GLFW
import Linear hiding (trace)
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as J

import Graphics.GlTF.Type
import Graphics.GlTF.Shader
import Graphics.GlTF.Animation
import Graphics.GlTF.Util
import Graphics.GlTF

--
-- Mouse handling
--
data MouseGrab = MouseGrab
  {
    mouseGrabPos :: V2 Float,
    mouseGrabMat :: M44 Float
  }

onMouseEvent ::
    (IORef (Maybe MouseGrab)) -> (IORef (M44 Float)) -> Window -> HIn.Chatter Int -> IO ()
onMouseEvent vGrab vMat win (HIn.Down _) =
  do
    mat <- readIORef vMat
    curPos <- runReaderT getCursorPos win
    let mgr = Just $ MouseGrab
          {
            mouseGrabPos = curPos,
            mouseGrabMat = mat
          }
    writeIORef vGrab mgr
onMouseEvent vGrab _ _ (HIn.Up _) = writeIORef vGrab Nothing

onMouseUpdate ::
    (MonadHolz env m, HasWindow env) =>
    (IORef (Maybe MouseGrab)) -> Float -> (IORef (M44 Float)) -> m ()
onMouseUpdate vGrab unitLen vMat = ($> ()) . runMaybeT $
  do
    Just MouseGrab{..} <- readIORef vGrab
    newPos <- getCursorPos
    let dif = newPos - mouseGrabPos
        r = norm dif
    guard $ r > 0.00001
    let
        V2 dx dy = dif ^/ r
        rot = axisAngle (V3 dy dx 0) (r / unitLen * pi)
        newMat = mkTransformation rot 0 !*! mouseGrabMat
    writeIORef vMat newMat

--
-- Utility
--
die :: MonadIO m => Text -> m a
die s = BS.putStr (encodeUtf8 s) >> exitFailure


-- data DrawState =

canvasSize :: V2 Float
canvasSize = V2 640 960

data App = App
  {
    appLogFunc :: LogFunc,
    appWindow :: Window,
    appShader :: MRShader
  }

instance HasShader App
  where
    type ShaderUniform App = MRUniform
    type ShaderVertex App = MRVertex
    getShader = appShader

instance HasWindow App
  where
    getWindow = appWindow

instance HasLogFunc App
  where
    logFuncL = lens appLogFunc (\x logFunc -> x{ appLogFunc = logFunc })

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

    let bbox = calcBBox gltf scene

    let tgtIx = 
            listToMaybe . catMaybes . V.toList $
                V.imap (\i x -> guard (nodeName x == Just (J.String "RightArm")) >> Just i) $
                    glTFNodes gltf

    logOpt <- logOptionsHandle stderr False

    withHolz $ withLogFunc logOpt $ \logFunc ->
      do
        w <- openWindow Windowed (Box (V2 0 0) canvasSize)
        shader <- makeVRMShader

        let app = App logFunc w shader
        runRIO app $
          do
            logInfo $ "RightArm >> " <> displayShow tgtIx 
            (vbs, _) <- mapToGL gltf bin scene
            aniState <- mapM (initAniState gltf bin) ani
            vTr <- newIORef (initTrans bbox)
            vT <-
              do
                t0 <- liftIO $ fromMaybe 0 <$> GLFW.getTime
                newIORef t0

            vMouseGrab <- newIORef Nothing
            linkMouseButton (onMouseEvent vMouseGrab vTr w)

            forever $ withFrame w $
              do
                windowShouldClose >>= bool (return ()) exitSuccess

                delta <-
                  do
                    t <- readIORef vT
                    t' <- liftIO $ fromMaybe 0 <$> GLFW.getTime
                    writeIORef vT t'
                    return $ realToFrac (t' - t)

                onMouseUpdate vMouseGrab ((canvasSize ^. _x) / 2) vTr
                tr <- readIORef vTr
                -- writeIORef vTr $! rotZX (delta * pi * 0.5) !*! tr

                initView
                setUniform model identity
                setUniform planeDir (V3 0.577 0.577 0.577)
                clearColor grayColor
                drawScene gltf vbs scene aniState delta

initView :: (MonadHolz r m, HasWindow r, HasMRShader r) => m ()
initView =
  do
    box@(Box (V2 x0 y0) (V2 x1 y1)) <- getBoundingBox
    setViewport box
    let w = x1 - x0
        h = y1 - y0
    setUniform projection $ ortho (-w/2) (w/2) (-h/2) (h/2) (-w/2) (w/2)
    setUniform sight (V3 0 0 (-1))


initTrans :: Box V3 Float -> M44 Float
initTrans bbox = mkTransformationMat (fromDiag33 r r (-r)) (V3 0.0 vOff 0.0)
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


grayColor :: V4 Float
grayColor = V4 0.7 0.7 0.7 1.0

blueColor :: V4 Float
blueColor = V4 0 0 1.0 (0.5)


dispEType :: (IsString s) => Int -> s
dispEType 5120 = "BYTE"
dispEType 5121 = "UNSIGNED_BYTE"
dispEType 5122 = "SHORT"
dispEType 5123 = "UNSIGNED_SHORT"
dispEType 5125 = "UNSIGNED_INT"
dispEType 5126 = "FLOAT"
dispEType _ = "Unknown"

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

