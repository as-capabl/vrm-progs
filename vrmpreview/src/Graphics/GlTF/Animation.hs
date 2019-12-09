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
    Graphics.GlTF.Animation
where

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
import Data.GlTF
import Linear hiding (trace)
import Foreign.Ptr
import Foreign.Storable

import Graphics.GlTF.Type

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

