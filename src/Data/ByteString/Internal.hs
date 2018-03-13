{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Internal 
  ( byteAt
  ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import qualified Data.ByteString as ByteString
import Data.Word
import Data.Bits (shiftL)
import Data.Maybe (fromMaybe)

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
