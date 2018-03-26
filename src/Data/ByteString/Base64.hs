{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Base64
  ( encode
  , decode
  
  , encodeURL
  , decodeURL
  ) where

import Data.Word
import Data.Bits (shiftL, shiftR, (.&.), (.|.))

import qualified Data.ByteString.Internal as Internal
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.BaseN

import Foreign.Ptr (plusPtr, Ptr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)

-- Pack a Word32 with a 6-bits at offset `off`. Offset is counted from the right
-- so `pack6 word 0 byte` packs the right-most 6 bits.
pack6 :: Int -> Word8 -> Word32 -> Word32
pack6 off word buf = buf .|. (fromIntegral word `shiftL` (off * 6))

-- Unpack 6 bits from a Word32 at offset `off`. Offset is counted form the right
-- so `unpack6 word 0` unpacks the 6 right-most bits into a Word8.
unpack6 :: Int -> Word32 -> Word8
unpack6 off buf = fromIntegral $ buf `shiftR` (6 * off) .&. 0x3f

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
encode = encodeAlphabet alphabet

decode :: B8.ByteString -> Either String B8.ByteString
decode = decodeAlphabet alphabet

encodeURL :: B8.ByteString -> B8.ByteString
encodeURL = encodeAlphabet alphabetURL

decodeURL :: B8.ByteString -> Either String B8.ByteString
decodeURL = decodeAlphabet alphabetURL

decodeAlphabet :: Enc -> B8.ByteString -> Either String B8.ByteString
decodeAlphabet enc src@(Internal.PS sfp soff slen)
  | drem /= 0 = Left "ByteString wrong length for valid Base64 encoding padding"
  | dlen <= 0 = Right B8.empty
  | otherwise = 
    unsafePerformIO $ byChunkErr 4 dlen onchunk onend src
    where
      (di, drem) = slen `divMod` 4
      dlen = di * 3

      onchunk :: Ptr Word8 -> Ptr Word8 -> IO (Either String Int)
      onchunk sp dp = do
        words <- traverse (peek . (sp `plusPtr`)) [0..3]
        case (unpack8x3 . pack6x4) <$> traverse (decodeWord enc) words of
          Left err -> return $ Left err
          Right decoded -> do
            pokeN dp 3 decoded
            return $ Right 3

      -- Maps number of padding bytes in source buffer to number of valid bytes
      -- to write to destination buffer.
      validBytes :: Int -> Either String Int
      validBytes 0 = Right 3
      validBytes 1 = Right 2
      validBytes 2 = Right 1
      validBytes n = Left $ "Invalid amount of padding: " ++ show n

      decodeBytes = mapM $ decodeWord enc

      onend :: Ptr Word8 -> Ptr Word8 -> Int -> IO (Either String Int)
      onend sp dp rem = do
        words <- traverse (peek . (sp `plusPtr`)) [0..3]
        let 
          npad = length $ takeWhile (==0x3d) $ reverse words
          decode = (unpack8x3 . pack6x4) <$> decodeBytes (take (4-npad) words)
        case (,) <$> validBytes npad <*> decode of
          Left err -> return $ Left err
          Right (n, decoded) -> do
            pokeN dp n decoded
            return $ Right $ dlen - (3 - n)


alphabet :: Enc
alphabet = mkEnc "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

alphabetURL :: Enc
alphabetURL = mkEnc "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

encodeAlphabet :: Enc -> B8.ByteString -> B8.ByteString
encodeAlphabet enc src@(Internal.PS sfp soff slen) =
  unsafePerformIO $ byChunk 3 dlen onchunk onend src

  where
    dlen = ((slen + 2) `div` 3) * 4

    onchunk sp dp = do
      w <- unpack6x4 . pack8x3 <$> traverse (peek . (sp `plusPtr`)) [0..2]
      pokeN dp 4 $ map (encodeWord enc) w
      return 4

    onend sp dp 0 = return ()
    onend sp dp rem = do
      w <- (unpack6x4 . pack8x3) <$> traverse (peek . (sp `plusPtr`)) [0..rem-1]
      let encoded = map (encodeWord enc) w
      poke dp               (encoded !! 0)
      poke (dp `plusPtr` 1) (encoded !! 1)
      poke (dp `plusPtr` 2) (if rem == 1 then 0x3d else encoded !! 2)
      poke (dp `plusPtr` 3) (0x3d :: Word8)




