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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

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
import qualified Graphics.Holz.Shader as Hz
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
data MRUniform h = MRUniform {
    model :: h (M44 Float),
    projection :: h (M44 Float),
    planeDir :: h (V3 Float),
    sight :: h (V3 Float),
    baseColorFactor :: h (V4 Float),
    metallic :: h Float,
    lamFactor :: h Float,
    nlamFactor :: h Float
  }
    deriving Generic

-- instance GStorable MRUniform where {}


data MRVertex = MRVertex {
    in_Position :: V3 Float,
    in_UV :: V2 Float,
    in_Normal :: V3 Float
  }
    deriving (Eq, Show, Generic)

-- instance GStorable MRVertex where {}

data MRFragment = MRFragment {
    texUV :: V2 Float,
    normal :: V3 Float,
    viewPos :: V4 Float,
    color :: V4 Float
  }
    deriving (Eq, Show, Generic)

-- instance GStorable MRFragment where {}

type MRShader = Hz.Shader MRUniform MRVertex

type HasMRShader r =
    (Hz.HasShader r, Hz.ShaderUniform r ~ MRUniform, Hz.ShaderVertex r ~ MRVertex)

{-
type MRShaderT m = ReaderT MRShader m

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
-}

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



