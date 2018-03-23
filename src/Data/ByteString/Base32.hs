{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Base32
  ( encode
  , decode

  , encodeHex
  , decodeHex
  ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as Internal
import Data.ByteString.BaseN

import Data.Word
import Data.Bits (shiftL, shiftR, (.|.), (.&.))

import Foreign.Ptr (plusPtr, Ptr)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)

pack5 :: Int -> Word8 -> Word64 -> Word64
pack5 off word buf = buf .|. (fromIntegral word `shiftL` (off * 5))

pack8x5 :: [Word8] -> Word64
pack8x5 bs = foldr (uncurry pack8) 0 (zip [4,3..0] bs)

unpack8x5 :: Word64 -> [Word8]
unpack8x5 w =
  map (`unpack8` w) [4,3..0]

unpack5 :: Int -> Word64 -> Word8
unpack5 off buf = fromIntegral $ buf `shiftR` (5 * off) .&. 0x1f

pack5x8 :: [Word8] -> Word64
pack5x8 bs = foldr (uncurry pack5) 0 (zip [7,6..0] bs)

unpack5x8 :: Word64 -> [Word8]
unpack5x8 bs =
  map (`unpack5` bs) [7,6..0]

padCeilN :: Int -> Int -> Int
padCeilN n x
  | remd == 0 = x
  | otherwise = (x - remd) + n
  where  mask = n - 1
         remd = x .&. mask

encodeHex :: B8.ByteString -> B8.ByteString
encodeHex = encodeAlphabet alphabetHex

decodeHex :: B8.ByteString -> Either String B8.ByteString
decodeHex = decodeAlphabet alphabetHex

encode :: B8.ByteString -> B8.ByteString
encode = encodeAlphabet alphabet

decode :: B8.ByteString -> Either String B8.ByteString
decode = decodeAlphabet alphabet

encodeAlphabet :: Enc -> B8.ByteString -> B8.ByteString
encodeAlphabet enc src@(Internal.PS sfp soff slen) =
  unsafePerformIO $ byChunk 5 dlen onchunk onend src
  where
    (d, m) = (slen * 8) `divMod` 5
    dlen   = padCeilN 8 (d + if m == 0 then 0 else 1)

    onchunk sp dp = do
      words <- unpack5x8 . pack8x5 <$> traverse peek (map (sp `plusPtr`) [0..4])
      pokeN dp 8 $ map (encodeWord enc) words
      return 8

    onend sp dp 0 = return ()
    onend sp dp rem = do
      words <- unpack5x8 . pack8x5 <$> sequence
        [ peek sp 
        , if rem > 1 then peek $ sp `plusPtr` 1 else return 0
        , if rem > 2 then peek $ sp `plusPtr` 2 else return 0
        , if rem > 3 then peek $ sp `plusPtr` 3 else return 0
        , if rem > 4 then peek $ sp `plusPtr` 4 else return 0
        ]
      let encoded = map (encodeWord enc) words
      poke dp               (encoded !! 0)
      poke (dp `plusPtr` 1) (encoded !! 1)
      poke (dp `plusPtr` 2) (if rem < 2 then 0x3d else encoded !! 2)
      poke (dp `plusPtr` 3) (if rem < 2 then 0x3d else encoded !! 3)
      poke (dp `plusPtr` 4) (if rem < 3 then 0x3d else encoded !! 4)
      poke (dp `plusPtr` 5) (if rem < 4 then 0x3d else encoded !! 5)
      poke (dp `plusPtr` 6) (if rem < 4 then 0x3d else encoded !! 6)
      poke (dp `plusPtr` 7) (0x3d :: Word8)

alphabet :: Enc
alphabet =  mkEnc "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

alphabetHex :: Enc
alphabetHex =  mkEnc "0123456789ABCDEFGHIJKLMNOPQRSTUV"

decodeAlphabet :: Enc -> B8.ByteString -> Either String B8.ByteString
decodeAlphabet enc src@(Internal.PS sfp soff slen)
  | slen `mod` 8 /= 0 = Left "ByteString wrong length for valid Base32 encoding"
  | slen == 0         = Right BS.empty
  | otherwise         =
      unsafePerformIO $ byChunkErr 8 dlen onchunk onend src
      where
        (d, m) = (slen * 5) `divMod` 8
        dlen   = d + if m == 0 then 0 else 1

        -- Maps number of padding bytes in source buffer to number of valid bytes
        -- to write to destination buffer.
        validBytes :: Int -> Either String Int
        validBytes 0 = Right 5
        validBytes 1 = Right 4
        validBytes 3 = Right 3
        validBytes 4 = Right 2
        validBytes 6 = Right 1
        validBytes n = Left $ "Invalid amount of padding: " ++ show n

        decodeBytes = mapM $ decodeWord enc

        onchunk :: Ptr Word8 -> Ptr Word8 -> IO (Either String Int)
        onchunk sp dp = do
          words <- traverse (peek . (sp `plusPtr`)) [0..7]
          case (unpack8x5 . pack5x8) <$> decodeBytes words of
            Left err -> return $ Left err
            Right decoded -> do
              pokeN dp 5 decoded
              return $ Right 5

        onend :: Ptr Word8 -> Ptr Word8  -> Int -> IO (Either String Int)
        onend sp dp rem = do
          words <- traverse (peek . (sp `plusPtr`)) [0..7]
          let
            npad = length $ takeWhile (==0x3d) $ reverse words
            decode = (unpack8x5 . pack5x8) <$> decodeBytes (take (8-npad) words)
          case (,) <$> validBytes npad <*> decode of
            Left err -> return $ Left err
            Right (n, decoded) -> do
              pokeN dp n decoded
              return $ Right $ dlen - (5 - n)
