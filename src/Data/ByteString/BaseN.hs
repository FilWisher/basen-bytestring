{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.BaseN
  ( Enc(..)
  , module Internal
  , mkEnc
  , encodeWord
  , decodeWord
  , pack8
  , unpack8
  , poke8s
  , byChunk
  , byChunkErr
  ) where

import qualified Data.ByteString.Internal as Internal
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Maybe (fromMaybe)
import Data.List (unfoldr)
import Control.Applicative (liftA3)

import Foreign.Ptr (plusPtr, minusPtr, castPtr, Ptr)
import Foreign.Storable (peek, poke, peekElemOff)
import Foreign.ForeignPtr (ForeignPtr(..), castForeignPtr, withForeignPtr)

-- Enc represents an encoding. The first ByteString is the map from decoded
-- bytes to encoded bytes.  The second ByteString is the map from encoded bytes
-- to decoded bytes. There is an optional `pad` character for padding encoded
-- strings.
--
-- The assumptions with Enc types is:
--  o That the encoded alphabet does not contain the byte 255 (it is used as a
--    sentinal)
--  o That the alphabet supplied to `mkEnc` is long enough for the encoding: no
--    bounds checking is done.
--
data Enc = Enc 
  { encmap  :: B8.ByteString
  , decmap  :: B8.ByteString
  , pad     :: B8.ByteString
  }
  deriving (Show)

-- Produce an `Enc` from an encoding alphabet. The decoding alphabet is derived
-- from the encoding alphabet. Non-present characters are signified with the
-- byte 0xFF. This means 0xFF cannot be present in the encoding alphabet.
mkEnc :: B8.ByteString -> B8.ByteString -> Enc
mkEnc table = Enc table dec
  where
    dec = BS.pack [ fromIntegral $ fromMaybe 0xFF (i `BS.elemIndex` table) | i <- [0..254] ]

encodeWord :: Enc -> Word8 -> Word8
encodeWord (Enc enc _ _) word = enc `BS.index` fromIntegral word

decodeWord :: Enc -> Word8 -> Either String Word8
decodeWord (Enc _ dec _) word = case dec `BS.index` fromIntegral word of
  255 -> Left ("Not valid encoding: " ++ show word)
  n   -> Right n

-- Pack a Word32 with a Word8 at offset `off`. Offset is counted from the right
-- so `pack8 word 0 byte` packs the word with the right-most byte:
pack8 :: Int -> Word8 -> Word32 -> Word32
pack8 off word buf = buf .|. (fromIntegral word `shiftL` (off * 8))

-- Unpack a byte from a Word32 at offset `off`. Offset is counted form the right
-- so `unpack8 word 0` unpacks the right-most byte.
unpack8 :: Int -> Word32 -> Word8
unpack8 off buf = fromIntegral $ buf `shiftR` (8 * off) .&. 0xff

unpack :: (Int -> Word32 -> Word8) -> Int -> Word32 -> [Word8]
unpack fn n word = unfoldr (build n (`fn` word)) 0
  where
    build :: Int -> (Int -> a) -> Int -> Maybe (a, Int)
    build limit fn n
      | n >= limit = Nothing
      | otherwise = Just (fn n, n + 1)

-- Push bytes to pointer in order.
poke8s :: Ptr Word8 -> [Word8] -> IO ()
poke8s ptr [] = return ()
poke8s ptr (x:xs) = poke ptr x >> poke8s (ptr `plusPtr` 1) xs

byChunk :: 
  Int -> 
  Int ->
  (Ptr Word8 -> Ptr Word8 -> IO Int) -> 
  (Ptr Word8 -> Ptr Word8 -> Int -> IO ()) -> 
  B8.ByteString -> 
  IO B8.ByteString
byChunk chunksize dlen onchunk onend (Internal.PS sfp soff slen) =
  withForeignPtr sfp $ \sptr -> do
    let
      end = sptr `plusPtr` (soff + slen)
      loop sp dp
        | sp `plusPtr` (chunksize-1) >= end = onend sp dp (end `minusPtr` sp)
        | otherwise               = do
          advance <- onchunk sp dp
          loop (sp `plusPtr` chunksize) (dp `plusPtr` advance)
    dfp <- Internal.mallocByteString dlen
    withForeignPtr dfp $ \dptr -> do
      loop sptr dptr
      return $ Internal.PS dfp 0 dlen

byChunkErr :: 
  Int ->
  Int ->
  (Ptr Word8 -> Ptr Word8 -> IO (Either String Int)) ->
  (Ptr Word8 -> Ptr Word8 -> Int -> IO (Either String Int)) ->
  B8.ByteString ->
  IO (Either String B8.ByteString)
byChunkErr chunksize dlen onchunk onend (Internal.PS sfp soff slen) =
  withForeignPtr sfp $ \sptr -> do
    let
      end = sptr `plusPtr` (soff + slen)
      loop sp dp
        | sp `plusPtr` chunksize > end  = return $ Left "Not enough bytes remaining"
        | sp `plusPtr` chunksize == end = onend sp dp (end `minusPtr` sp)
        | otherwise               = do
          advance <- onchunk sp dp
          case advance of
            Left err -> return $ Left err
            Right n -> loop (sp `plusPtr` chunksize) (dp `plusPtr` n)
    dfp <- Internal.mallocByteString dlen
    fmap (Internal.PS dfp 0) <$> withForeignPtr dfp (loop sptr)
