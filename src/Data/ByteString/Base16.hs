{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Base16
  ( Base16
  , decodeAlphabet
  , encodeAlphabet
  , decode
  , encode
  , hexupper
  , hexlower
  ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Internal as Internal

import System.IO.Unsafe (unsafePerformIO)
import Foreign.Storable (peek, poke, peekElemOff)
import Foreign.Ptr (plusPtr, castPtr, Ptr, minusPtr)

import Data.ByteString.Internal.Extended (byChunk, byChunkErr, Enc(..), encodeWord, decodeWord, mkEnc)
import           Data.Bits             as Bits (shiftL, shiftR, (.&.), (.|.))
import           Data.Word
import Control.Monad (void)

type Base16 = B8.ByteString

encode :: B8.ByteString -> B8.ByteString
encode = encodeAlphabet hexlower

decode :: B8.ByteString -> Either String B8.ByteString
decode = decodeAlphabet hexlower

poke8 :: Ptr Word8 -> Word8 -> IO ()
poke8 = poke

encodeAlphabet :: Enc -> B8.ByteString -> B8.ByteString
encodeAlphabet enc src@(Internal.PS sfp soff slen) =
  unsafePerformIO $ byChunk 1 (slen * 2) onchunk onend src
  where
    encodeNibble :: (Word8 -> Word8) -> Ptr Word8 -> IO Word8
    encodeNibble fn p = encodeWord enc . fn <$> peek p

    onchunk :: Ptr Word8 -> Ptr Word8 -> IO Int
    onchunk sp dp = do
      poke8 dp               =<< encodeNibble leftNibble  sp
      poke8 (dp `plusPtr` 1) =<< encodeNibble rightNibble sp
      return 2
    
    onend :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
    onend sp dp rem = return ()

decodeAlphabet :: Enc -> B8.ByteString -> Either String B8.ByteString
decodeAlphabet enc src@(Internal.PS sfp soff slen) =
  unsafePerformIO $ byChunkErr 2 (slen `div` 2) onchunk onend Left src
  where
    onchunk :: Ptr Word8 -> Ptr Word8 -> IO (Either String Int)
    onchunk sp dp = do
      leftNibble  <- decodeWord enc <$> peek sp
      rightNibble <- decodeWord enc <$> peek (sp `plusPtr` 1)
      case (.|.) <$> fmap (`Bits.shiftL` 4) leftNibble <*> rightNibble of
        Left err -> do
          l <- peek sp :: IO Word8
          r <- peek (sp `plusPtr` 1) :: IO Word8
          return $ Left $ "Invalid byte encountered. One of: " ++ B8.unpack (BS.pack [l,r])
        Right w -> do
          poke8 dp w
          return $ Right 1

    onend :: Ptr Word8 -> Ptr Word8  -> Int -> IO (Either String Int)
    onend sp dp rem = return $ Right 0

leftNibble :: Word8 -> Word8
leftNibble n = n `Bits.shiftR` 4

rightNibble :: Word8 -> Word8
rightNibble n = 0x0F .&. n

hexlower :: Enc
hexlower = mkEnc "0123456789abcdef" ""

hexupper :: Enc
hexupper = mkEnc "0123456789ABCDEF" ""
