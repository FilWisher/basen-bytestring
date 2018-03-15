{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Base64 where

import Data.Bits       as Bits (shiftR, (.&.), (.|.), shiftL)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import           Data.Word
import Data.Monoid
import qualified Data.ByteString.Internal.Extended as BI

type Base64 = B8.ByteString

type Word6 = Word8

-- Base64 encoded string should always be a multiple of four bytes.
toWord8 :: [Word8] -> Maybe [Word8]
toWord8 bs = case bs of
  [] -> Just []
  [one,two,three,four] -> Just
    [ shiftL one 2   .|. shiftR two 4
    , shiftL two 4   .|. shiftR three 2
    , shiftL three 6 .|. BI.clearL four 2
    ]
  _                 -> Nothing

-- FIXME: need to handle end of stream properly: don't decode paddings
-- FIXME: need to check if 255 was returned in which case it failed
-- TODO:  work in Either instead of Maybe to give better response
decode :: BI.Enc -> B8.ByteString -> Maybe B8.ByteString
decode enc bs = case toWord8 $ map (BI.decodeWord enc) (BS.unpack before) of
  Just [] -> Just ""
  Just words -> fmap (B8.append $ BS.pack words) (decode enc after)
  Nothing -> Nothing
  where
    (before, after) = B8.splitAt 4 bs

base64 :: B8.ByteString
base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

enc :: BI.Enc
enc = BI.mkEnc base64 "="

encode :: BI.Enc -> B8.ByteString -> B8.ByteString
encode enc bs = case toWord6 before of
  [] -> ""
  words -> BS.pack (map (BI.encodeWord enc) words) `B8.append`
    if length words < 4 then
      B8.concat $ replicate (4 - length words) (BI.pad enc)
    else
      encode enc after
  where
    (before, after) = B8.splitAt 3 bs

toWord6 :: B8.ByteString -> [Word8]
toWord6 bs = case BS.unpack bs of
  []                -> []
  [one]             -> [ shiftR one 2, BI.clearL (shiftL one 4) 2 ]
  [one, two]        -> [ shiftR one 2
                       , BI.clearL (shiftL one 4) 2 .|. shiftR two 4
                       , BI.clearL (shiftL two 2) 2
                       ]
  [one, two, three] -> [ shiftR one 2
                       , BI.clearL (shiftL one 4) 2 .|. shiftR two 4
                       , BI.clearL (shiftL two 2) 2 .|. shiftR three 6
                       , BI.clearL three 2
                       ]
  _                 -> error "Too many bytes"
