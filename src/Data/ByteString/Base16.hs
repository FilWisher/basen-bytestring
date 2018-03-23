{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Base16
  ( encode
  , decode
  ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Internal as Internal

import Foreign.Storable (peek, poke)
import Foreign.Ptr (plusPtr, Ptr)
import System.IO.Unsafe (unsafePerformIO)

import Data.ByteString.BaseN
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word

encode :: B8.ByteString -> B8.ByteString
encode = encodeAlphabet hexlower

decode :: B8.ByteString -> Either String B8.ByteString
decode = decodeAlphabet hexlower

encodeAlphabet :: Enc -> B8.ByteString -> B8.ByteString
encodeAlphabet enc src@(Internal.PS sfp soff slen) =
  unsafePerformIO $ byChunk 1 (slen * 2) onchunk onend src
  where
    encodeNibble :: (Word8 -> Word8) -> Ptr Word8 -> IO Word8
    encodeNibble fn p = encodeWord enc . fn <$> peek p

    onchunk :: Ptr Word8 -> Ptr Word8 -> IO Int
    onchunk sp dp = do
      poke dp               =<< encodeNibble leftNibble  sp
      poke (dp `plusPtr` 1) =<< encodeNibble rightNibble sp
      return 2
    
    onend :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
    onend sp dp rem = return ()

decodeAlphabet :: Enc -> B8.ByteString -> Either String B8.ByteString
decodeAlphabet enc src@(Internal.PS sfp soff slen)
  | slen == 0 = Right BS.empty
  | otherwise = 
    unsafePerformIO $ byChunkErr 2 dlen onchunk onend src
  where
    dlen = slen `div` 2

    onchunk :: Ptr Word8 -> Ptr Word8 -> IO (Either String Int)
    onchunk sp dp = do
      leftNibble  <- decodeWord enc <$> peek sp
      rightNibble <- decodeWord enc <$> peek (sp `plusPtr` 1)
      case (.|.) <$> fmap (`shiftL` 4) leftNibble <*> rightNibble of
        Left err -> do
          l <- peek sp :: IO Word8
          r <- peek (sp `plusPtr` 1) :: IO Word8
          return $ Left $ "Invalid byte encountered. One of: " ++ B8.unpack (BS.pack [l,r])
        Right w -> do
          poke dp w
          return $ Right 1

    onend :: Ptr Word8 -> Ptr Word8  -> Int -> IO (Either String Int)
    onend sp dp rem = onchunk sp dp >> return (Right dlen)

leftNibble :: Word8 -> Word8
leftNibble n = n `shiftR` 4

rightNibble :: Word8 -> Word8
rightNibble n = 0x0F .&. n

hexlower :: Enc
hexlower = mkEnc "0123456789abcdef"

hexupper :: Enc
hexupper = mkEnc "0123456789ABCDEF"
