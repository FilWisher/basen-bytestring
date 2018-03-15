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

import Data.ByteString.Internal.Extended (byteAt)
import           Data.Bits             as Bits (shiftL, shiftR, (.&.), (.|.))
import           Data.Word

type Base16 = B8.ByteString

encode :: B8.ByteString -> Either String B8.ByteString
encode = encodeAlphabet hexlower

decode :: B8.ByteString -> Either String B8.ByteString
decode = decodeAlphabet hexlower

encodeAlphabet :: B8.ByteString -> B8.ByteString -> Either String B8.ByteString
encodeAlphabet table 
  | B8.length table /= 16 = const $ Left tableError
  | otherwise = 
      BS.foldr (\byte encoded -> BS.append <$> toHex byte <*> encoded) (Right "")
  where
    tableError :: String
    tableError = "Encoding table not large enough to encode byte"

    byteFor    :: Word8 -> Maybe B8.ByteString
    byteFor = fmap BS.singleton . (table `byteAt`) . fromIntegral

    left :: Word8 -> Maybe B8.ByteString
    left = byteFor . leftNibble
    
    right :: Word8 -> Maybe B8.ByteString
    right = byteFor . rightNibble

    toHex :: Word8 -> Either String B8.ByteString
    toHex word = 
      case B8.append <$> left word <*> right word of
        Nothing    -> Left tableError
        Just octet -> Right octet

decodeAlphabet :: B8.ByteString -> B8.ByteString -> Either String B8.ByteString
decodeAlphabet table bs = 
  case snd $ BS.foldr decodeStep (Nothing, Just "") bs of
    Nothing -> Left "Invalid base16 string"
    Just str -> Right str 
  where
    getNibble :: Word8 -> Maybe Word8
    getNibble = fmap fromIntegral . flip BS.elemIndex table

    getWord :: Word8 -> Word8 -> Maybe Word8
    getWord l r = (.|.) <$> (flip Bits.shiftL 4 <$> getNibble l) <*> getNibble r

    decodeStep l (Nothing,bs) = (Just l, bs)
    decodeStep l (Just r,bs) = (Nothing, BS.cons <$> getWord l r <*> bs)

leftNibble :: Word8 -> Word8
leftNibble = flip Bits.shiftR 4

rightNibble :: Word8 -> Word8
rightNibble n = 0x0F .&. n

hexlower :: B8.ByteString
hexlower = B8.pack $ ['0'..'9'] ++ ['a'..'f']

hexupper :: B8.ByteString
hexupper = B8.pack $ ['0'..'9'] ++ ['A'..'F']
