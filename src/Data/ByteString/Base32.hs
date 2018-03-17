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
  ) where

-- TODO: use pack8 :: Num a => Word8 -> a -> a and specialize
-- FIXME: single character encoding is failing: something is very wrong

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
pack8x5 bs = foldr (uncurry pack8) 0 (zip [0..4] bs)

unpack8x5 :: Word64 -> [Word8]
unpack8x5 w =
  map (`unpack8` w) [0..4]

pack5 :: Int -> Word8 -> Word64 -> Word64
pack5 off word buf = buf .|. (fromIntegral word `shiftL` (off * 5))

unpack5 :: Int -> Word64 -> Word8
unpack5 off buf = fromIntegral $ buf `shiftR` (5 * off) .&. 0x1f

pack5x8 :: [Word8] -> Word64
pack5x8 bs = foldr (uncurry pack5) 0 (zip [0..7] bs)

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

encode :: Enc -> B8.ByteString -> B8.ByteString
encode enc src@(Internal.PS sfp soff slen) =
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
      print rem
      w <- pack8x5 <$> sequence
        [ peek sp 
        , if rem >= 2 then peek $ sp `plusPtr` 1 else return 0
        , if rem <= 3 then peek $ sp `plusPtr` 2 else return 0
        , if rem <= 4 then peek $ sp `plusPtr` 3 else return 0
        , if rem <= 5 then peek $ sp `plusPtr` 4 else return 0
        ]
      let bytes = map (encodeWord enc) $ unpack5x8 w
      print bytes
      print (BS.pack bytes)
      poke8 dp               (bytes !! 0)
      poke8 (dp `plusPtr` 1) (if rem == 1 then 0x3d else bytes !! 1 )
      poke8 (dp `plusPtr` 2) (if rem <= 2 then 0x3d else bytes !! 2 )
      poke8 (dp `plusPtr` 3) (if rem <= 2 then 0x3d else bytes !! 3 )
      poke8 (dp `plusPtr` 4) (if rem <= 2 then 0x3d else bytes !! 4 )
      poke8 (dp `plusPtr` 5) (if rem <= 3 then 0x3d else bytes !! 5 )
      poke8 (dp `plusPtr` 6) (if rem <= 3 then 0x3d else bytes !! 6 )
      poke8 (dp `plusPtr` 7) (if rem <= 4 then 0x3d else bytes !! 7 )

enc :: Enc
enc =  mkEnc "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567" ""

quickEncode = BS.pack . map (encodeWord enc) . unpack5x8 . pack8x5 . BS.unpack
quickEncode' :: B8.ByteString -> [Word8]
quickEncode' = unpack5x8 . pack8x5 . BS.unpack
