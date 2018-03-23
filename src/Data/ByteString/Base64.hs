{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Base64
  ( encode
  , decode
  , enc
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word
import Data.List (unfoldr)

import Data.ByteString.BaseN
import qualified Data.ByteString.Internal as Internal
import qualified Data.ByteString.Char8 as B8

import Foreign.Ptr (plusPtr, castPtr, Ptr, minusPtr)
import Foreign.ForeignPtr (ForeignPtr(..), castForeignPtr, withForeignPtr)
import Foreign.Storable (peek, poke, peekElemOff)
import System.IO.Unsafe (unsafePerformIO)

poke8 :: Ptr Word8 -> Word8 -> IO ()
poke8 = poke

pack8x3 :: [Word8] -> Word32
pack8x3 bs = foldr (uncurry pack8) 0 (zip [2,1,0] bs)

unpack8x3 :: Word32 -> [Word8]
unpack8x3 w =
  map (`unpack8` w) [2,1,0]

pack6x4 :: [Word8] -> Word32
pack6x4 bs = foldr (uncurry pack6) 0 (zip [3,2..0] bs)

unpack6x4 :: Word32 -> [Word8]
unpack6x4 bs =
  map (`unpack6` bs) [3,2..0]

encode :: B8.ByteString -> B8.ByteString
encode = encodeAlphabet enc

decode :: B8.ByteString -> Either String B8.ByteString
decode = decodeAlphabet enc

decodeAlphabet :: Enc -> B8.ByteString -> Either String B8.ByteString
decodeAlphabet enc src@(Internal.PS sfp soff slen)
  | drem /=0 = Left "Invalid padding"
  | dlen <= 0 = Right B8.empty
  | otherwise = 
    unsafePerformIO $ byChunkErr 4 dlen onchunk onend src
    where
      (di, drem) = slen `divMod` 4
      dlen = di * 3
      
      defaults :: Word8 -> Word8
      defaults x = if x == 0x3d then 0 else x

      look :: Ptr Word8 -> IO Word8
      look = fmap defaults . peek


      onchunk :: Ptr Word8 -> Ptr Word8 -> IO (Either String Int)
      onchunk sp dp = do
        w <- traverse (look . (sp `plusPtr`)) [0..3]
        let decoded = (unpack8x3 . pack6x4) 
              <$> traverse (decodeWord enc) w
        case decoded of
          Left err -> return $ Left err
          Right decoded' -> do
            mapM_ (\(off, v) -> poke (dp `plusPtr` off) v) (zip [0..2] decoded')
            return $ Right 3

      nValidBytes 0 = 3
      nValidBytes 1 = 2
      nValidBytes 2 = 1
      paddingLength = length . takeWhile (==0x3d) . reverse

      onend :: Ptr Word8 -> Ptr Word8 -> Int -> IO (Either String Int)
      onend sp dp rem = do
        w <- traverse (peek . (sp `plusPtr`)) [0..3]
        let 
          npadding = paddingLength w
          decoded = (unpack8x3 . pack6x4) 
              <$> traverse (decodeWord enc) (take (4-npadding) w)
        case decoded of
          Left err -> return $ Left err
          Right decoded' -> do
            mapM_ 
              (\(off, v) -> poke (dp `plusPtr` off) v) 
              (zip [0..(nValidBytes npadding - 1)] decoded')
            return $ Right (dlen - (3 - nValidBytes npadding))


base64 :: B8.ByteString
base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

enc :: Enc
enc = mkEnc base64 "="


-- Pack a Word32 with a 6-bits at offset `off`. Offset is counted from the right
-- so `pack6 word 0 byte` packs the right-most 6 bits.
pack6 :: Int -> Word8 -> Word32 -> Word32
pack6 off word buf = buf .|. (fromIntegral word `shiftL` (off * 6))

-- Unpack 6 bits from a Word32 at offset `off`. Offset is counted form the right
-- so `unpack6 word 0` unpacks the 6 right-most bits into a Word8.
unpack6 :: Int -> Word32 -> Word8
unpack6 off buf = fromIntegral $ buf `shiftR` (6 * off) .&. 0x3f

encodeAlphabet :: Enc -> B8.ByteString -> B8.ByteString
encodeAlphabet enc src@(Internal.PS sfp soff slen) =
  unsafePerformIO $ byChunk 3 dlen onchunk onend src

  where
    dlen = ((slen + 2) `div` 3) * 4

    onchunk sp dp = do
      w <- unpack6x4 . pack8x3 <$> traverse (peek . (sp `plusPtr`)) [0..2]
      poke8s dp $ map (encodeWord enc) w
      return 4

    onend sp dp 0 = onchunk sp dp >> return ()
    onend sp dp rem = do
      w <- (unpack6x4 . pack8x3) <$> traverse (peek . (sp `plusPtr`)) [0..rem-1]
      let bytes = map (encodeWord enc) w
      poke8 dp               (bytes !! 0)
      poke8 (dp `plusPtr` 1) (bytes !! 1)
      poke8 (dp `plusPtr` 2) (if rem == 1 then 0x3d else bytes !! 2)
      poke8 (dp `plusPtr` 3) 0x3d
