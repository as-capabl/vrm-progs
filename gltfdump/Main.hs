{-# LANGUAGE LambdaCase #-}

module Main where

import RIO
import qualified RIO.ByteString as BS
import Data.GlTF
import System.Environment (getArgs)

die e = logError e >> exitFailure

main :: IO ()
main = runSimpleApp $
  do
    fname : _ <- liftIO getArgs
    (gltf, bin) <- liftIO (readGlbRaw fname) >>= \case
        Left err -> die (display err)
        Right x -> return x
    BS.hPut stdout $ encodeUtf8 . utf8BuilderToText $ displayShow gltf
    return ()
