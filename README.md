# basen-bytestring

Base N ByteStrings for the common cases:

```
Data.ByteString.Base16
Data.ByteString.Base32
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

Currently `Data.ByteString.Base58` is not implemented although it is on the
roadmap.

The core `Data.ByteString.BaseN` contains all the common utilities for packing
bytes, processing ByteStrings, creating encodings, and encoding/decoding single
bytes.
