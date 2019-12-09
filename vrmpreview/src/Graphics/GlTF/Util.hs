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
    Graphics.GlTF.Util
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
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Graphics.GL
import Linear hiding (trace)
import Foreign.Ptr
import Foreign.Marshal.Utils (with)

sizeOfGLType GL_UNSIGNED_BYTE = 1
sizeOfGLType GL_UNSIGNED_SHORT = 2
sizeOfGLType GL_UNSIGNED_INT = 4
sizeOfGLType GL_FLOAT = 4
sizeOfGLType x = error $ "sizeOfGLType " ++ show x

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

setUniform3FV :: MonadIO m => GLint -> V3 Float -> m ()
setUniform3FV loc col = liftIO $ with col $ glUniform3fv loc 1 . castPtr

setUniform4FV :: MonadIO m => GLint -> V4 Float -> m ()
setUniform4FV loc col = liftIO $ with col $ glUniform4fv loc 1 . castPtr

