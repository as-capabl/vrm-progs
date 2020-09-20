{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import RIO
import RIO.Vector ((!?), imapM_)
import RIO.File (writeBinaryFile)
import qualified RIO.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T
import Control.Monad.Except
import Data.GlTF
import System.Environment (getArgs)
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Data.Aeson as J

die e = logError e >> exitFailure

main :: IO ()
main = runSimpleApp $
  do
    fname <- liftIO getArgs >>= \case
        fname : _ -> return fname
        _ -> exitFailure
    (gltf, bin) <- liftIO (readGlbRaw fname) >>= \case
        Left err -> die (display err)
        Right x -> return x
    -- BS.hPut stdout $ encodeUtf8 . utf8BuilderToText $ displayShow gltf
    BL.hPut stdout gltf
    -- saveBuffers gltf bin
    return ()

saveBuffers gltf bin = imapM_ go $ glTFImages gltf
  where
    go ix img = warnOnException $
      do
        let bviewIdx = imageBufferView img
        bview <-
            maybe (throwError "No imageBufferView") return $
                bviewIdx >>= (glTFBufferViews gltf !?)
        let ofs = fromMaybe 0 $ bufferViewByteOffset bview
            len = bufferViewByteLength bview
            mime = imageMimeType img 
            extn = if
                | mime == Just (J.String "image/png") -> ".png"
                | otherwise -> ""
            imgName = case imageName img
              of
                Just (J.String s) -> display s
                _ -> mempty
            fname = T.unpack $ utf8BuilderToText $
                displayShow ix <> "_" <> imgName <> extn
        liftIO $ withForeignPtr bin $ \p ->
          do
            b <- BS.unsafePackCStringLen (p `plusPtr` ofs, len)
            writeBinaryFile fname b

warnOnException ::
    (MonadIO m, HasLogFunc env, MonadReader env m, HasCallStack) =>
    ExceptT Utf8Builder m a -> m (Maybe a)
warnOnException mx = runExceptT mx >>= \case
    Left s -> logWarn s >> return Nothing
    Right x -> return $ Just x
