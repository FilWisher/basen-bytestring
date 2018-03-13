module Data.ByteString.Base64 where

import qualified Data.ByteString as BS

type Base64 = BS.ByteString

encode :: BS.ByteString -> Base64 -> BS.ByteString
encode table base64 = undefined

decode :: BS.ByteString -> BS.ByteString -> Either String Base64
decode table bs = undefined
