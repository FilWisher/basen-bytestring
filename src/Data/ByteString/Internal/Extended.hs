{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Internal.Extended
  ( Enc(..)
  , byteAt
  , module Internal
  , table
  , mkEnc
  , encodeWord
  , decodeWord
  , clearL
  ) where

import qualified Data.ByteString.Internal as Internal
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import qualified Data.ByteString as ByteString
import Data.Word
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Maybe (fromMaybe)

import Foreign.Ptr (plusPtr, castPtr)
import Foreign.ForeignPtr (ForeignPtr(..), castForeignPtr, withForeignPtr)
import Foreign.Storable (peek, poke, peekElemOff)
import System.IO.Unsafe (unsafePerformIO)

bs2i :: B8.ByteString -> Integer
bs2i = ByteString.foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0

encode :: B8.ByteString -> B8.ByteString -> Either String B8.ByteString
encode table str = go n (Right "")
  where
      n = bs2i str
      (n', mod) = n `divMod` 58
      -- go 0 enc = enc
      -- go n enc =
      --   case table `byteAt` fromIntegral mod of
      --     Nothing -> Left "Badly formatted ByteString"
      --     Just byte -> go n' (fmap (byte `ByteString.cons`) enc)
      go n enc =
        case table `byteAt` fromIntegral mod of
          Nothing -> Left "Badly formatted ByteString"
          Just byte -> go n' (fmap (byte `ByteString.cons`) enc)

table :: B8.ByteString
table = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

byteAt :: B8.ByteString -> Int -> Maybe Word8
byteAt bs n
  | B8.length bs <= n = Nothing
  | otherwise = Just $ ByteString.index bs n


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

decodeWord :: Enc -> Word8 -> Word8
decodeWord (Enc _ dec _) word = dec `BS.index` fromIntegral word

clearL :: Word8 -> Int -> Word8
clearL word n = shiftR (shiftL word n) n
