# basenstrings

Base N ByteStrings for the common cases:

```
Data.ByteString.Base16
Data.ByteString.Base32
Data.ByteString.Base58
Data.ByteString.Base64
```

All with the same API:

```
module Data.ByteString.BaseN where

import qualified Data.ByteString.Char8 as B8

encode         :: B8.ByteString -> B8.ByteString
decode         :: B8.ByteString -> B8.ByteString

encodeAlphabet :: Enc -> B8.ByteString -> B8.ByteString
decodeAlphabet :: Enc -> B8.ByteString -> B8.ByteString
```
