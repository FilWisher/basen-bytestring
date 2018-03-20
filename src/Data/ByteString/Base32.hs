{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Base32
  ( pack8
  , unpack8
  , pack5
  , unpack5
  , pack8x5
  , unpack8x5
  , pack5x8
  , unpack5x8

  , encodeAlphabet
  , decodeAlphabet

  , encode
  , decode
  , alphabet

  , encodeHex
  , decodeHex
  , alphabetHex
  ) where

-- TODO: use pack8 :: Num a => Word8 -> a -> a and specialize

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as Internal
import Data.ByteString.Internal.Extended hiding (unpack8, pack8) 

import Data.Word
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Control.Monad (liftM5)

import Foreign.Ptr (plusPtr, minusPtr, castPtr, Ptr)
import Foreign.Storable (peek, poke, peekElemOff)
import Foreign.ForeignPtr (ForeignPtr(..), castForeignPtr, withForeignPtr)
import System.IO.Unsafe (unsafePerformIO)

pack8 :: Int -> Word8 -> Word64 -> Word64
pack8 off word buf = buf .|. (fromIntegral word `shiftL` (off * 8))

unpack8 :: Int -> Word64 -> Word8
unpack8 off buf = fromIntegral $ buf `shiftR` (8 * off) .&. 0xff

pack8x5 :: [Word8] -> Word64
pack8x5 bs = foldr (uncurry pack8) 0 (zip [4,3..0] bs)

unpack8x5 :: Word64 -> [Word8]
unpack8x5 w =
  map (`unpack8` w) [4,3..0]

pack5 :: Int -> Word8 -> Word64 -> Word64
pack5 off word buf = buf .|. (fromIntegral word `shiftL` (off * 5))

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

poke8 :: Ptr Word8 -> Word8 -> IO ()
poke8 = poke

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
      w <- pack8x5 <$> traverse peek (map (sp `plusPtr`) [0..4])
      poke8s dp 
        $ map (encodeWord enc) 
        $ unpack5x8 w
      return 8

    onend sp dp 0 = return ()
    onend sp dp rem = do
      w <- pack8x5 <$> sequence
        [ peek sp 
        , if rem > 1 then peek $ sp `plusPtr` 1 else return 0
        , if rem > 2 then peek $ sp `plusPtr` 2 else return 0
        , if rem > 3 then peek $ sp `plusPtr` 3 else return 0
        , if rem > 4 then peek $ sp `plusPtr` 4 else return 0
        ]
      let bytes = map (encodeWord enc) $ unpack5x8 w
      poke8 dp               (bytes !! 0)
      poke8 (dp `plusPtr` 1) (bytes !! 1)
      poke8 (dp `plusPtr` 2) (if rem < 2 then 0x3d else bytes !! 2)
      poke8 (dp `plusPtr` 3) (if rem < 2 then 0x3d else bytes !! 3)
      poke8 (dp `plusPtr` 4) (if rem < 3 then 0x3d else bytes !! 4)
      poke8 (dp `plusPtr` 5) (if rem < 4 then 0x3d else bytes !! 5)
      poke8 (dp `plusPtr` 6) (if rem < 4 then 0x3d else bytes !! 6)
      poke8 (dp `plusPtr` 7) 0x3d

alphabet :: Enc
alphabet =  mkEnc "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567" ""

-- TODO: actually use hex
alphabetHex :: Enc
alphabetHex =  mkEnc "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567" ""

-- TODO: this is will throw errors on invalid numbers of padding
-- TODO: clean this up
decodeAlphabet :: Enc -> B8.ByteString -> Either String B8.ByteString
decodeAlphabet enc src@(Internal.PS sfp soff slen)
  | slen `mod` 8 /= 0 = Left "ByteString wrong length for valid Base32 encoding"
  | slen == 0         = Right BS.empty
  | otherwise         =
      unsafePerformIO $ byChunkErr 8 dlen onchunk onend src
      where
        (d, m) = (slen * 5) `divMod` 8
        dlen   = d + if m == 0 then 0 else 1

        defaults :: Word8 -> Word8
        defaults x = if x == 0x3d then 0 else x
        look :: Ptr Word8 -> IO Word8
        look = fmap defaults . peek
        paddingLength = length . takeWhile (==0x3d) . reverse

        nValidBytes 0 = 5
        nValidBytes 1 = 4
        nValidBytes 3 = 3
        nValidBytes 4 = 2
        nValidBytes 6 = 1

        onchunk :: Ptr Word8 -> Ptr Word8 -> IO (Either String Int)
        onchunk sp dp = do
          w <- traverse (look . (sp `plusPtr`)) [0..7]
          let decoded = (unpack8x5 . pack5x8) 
                <$> mapM (decodeWord enc) w
          case decoded of
            Left err -> return $ Left err
            Right decoded' -> do
              mapM_ (\(off, v) -> poke (dp `plusPtr` off) v) (zip [0..4] decoded')
              return $ Right 5

        onend :: Ptr Word8 -> Ptr Word8  -> Int -> IO (Either String Int)
        onend sp dp rem = do
          w <- traverse (peek . (sp `plusPtr`)) [0..7]
          let npadding = paddingLength w
              decoded = (unpack8x5 . pack5x8)
                <$> mapM (decodeWord enc) (take (8-npadding) w)
          case decoded of
            Left err -> return $ Left err
            Right decoded' -> do
              mapM_ 
                (\(off, v) -> (poke (dp `plusPtr` off) v)) 
                (zip [0..(nValidBytes npadding - 1)] (decoded' ++ replicate npadding 0))
              return $ Right (dlen - (5 - nValidBytes npadding))
