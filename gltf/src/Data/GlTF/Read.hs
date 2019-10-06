{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module
    Data.GlTF.Read
      (
        BinChunk,
        withBin,
        readGlbRaw
      )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bifunctor (first, second)
import Data.Int (Int32)
import Control.Monad.Except
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import System.IO
import Data.GlTF.GlTF
import qualified Data.Aeson as J

type BinChunk = ForeignPtr ()

withBin :: BinChunk -> Int -> Int -> (Ptr a -> IO b) -> IO b
withBin c ofs _ action = withForeignPtr c $ \p -> action (p `plusPtr` ofs)

magic :: ByteString
magic = "glTF"

readGlbRaw :: FilePath -> IO (Either Text (GlTF, BinChunk))
readGlbRaw fname = withBinaryFile fname ReadMode $ \h -> runExceptT $ 
  do
    -- Check magic bytes.
    mg <- lift $ BS.hGet h (BS.length magic)
    guardExcept (mg == magic) "Magic bytes not match."

    -- Check version
    version <- hGetStorable @Int32 h
    guardExcept (version == 2) "Version not supported."

    fsize <- hGetStorable @Int32 h

    (_, cjson) <- readChunk h (liftIO . BL.hGet h)
    (_, bin) <- readChunk h (liftIO . hGetForeignPtr h)

    case J.eitherDecode cjson
      of
        Left err -> throwError $ T.pack err
        Right j -> return (j, bin)
  where
    readChunk h f =
      do
        csize <- hGetStorable @Int32 h
        ctype <- lift $ BS.hGet h 4
        ccontent <- f $ fromIntegral csize
        return (ctype, ccontent)
    
    hGetForeignPtr h len =
      do
        ret <- mallocForeignPtrBytes len
        withForeignPtr ret $ \p ->
            hGetBuf h p len
        return ret
        


guardExcept :: MonadError e m => Bool -> e -> m ()
guardExcept True e = return ()
guardExcept False e = throwError e

hGetStorable :: forall a m. (MonadIO m, Storable a) => Handle -> m a
hGetStorable h = liftIO $ alloca $ \p -> -- mallocForegiPtr is faster?
  do
    hGetBuf h p (sizeOf @a undefined)
    peek p