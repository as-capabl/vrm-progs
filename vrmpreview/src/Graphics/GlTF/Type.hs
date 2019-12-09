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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module
    Graphics.GlTF.Type
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
import Graphics.GL
import Foreign.Storable.Generic


data MappedPrim = MappedPrim {
    releasePrim :: IO (),
    drawPrim :: IO ()
  }

type MeshMap = Vector (Vector MappedPrim)

type TxMap = IntMap GLuint

--
-- Shader
--
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

--
-- Animation
--
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



