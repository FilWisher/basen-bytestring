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

type BaseN = B8.ByteString

encode         :: BaseN -> B8.ByteString
decode         :: BaseN -> B8.ByteString

encodeAlphabet :: B8.ByteString -> BaseN -> B8.ByteString
decodeAlphabet :: B8.ByteString -> BaseN -> B8.ByteString
```
