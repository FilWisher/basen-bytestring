{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck (Arbitrary(..), choose, forAll)
import Test.QuickCheck.All (quickCheckAll)
import Control.Monad (void)
import Data.Word

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base32 as Base32
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as B8

instance Arbitrary B8.ByteString where
  arbitrary = B8.pack <$> arbitrary

prop_base64_inverse :: B8.ByteString -> Bool
prop_base64_inverse bs =
  either (const False) (bs==)
    $ Base64.decode Base64.enc $ Base64.encode Base64.enc bs

prop_pack8_inverse :: Word8 -> Bool
prop_pack8_inverse word =
  all (\n -> Base32.unpack8 n (Base32.pack8 n word 0) == word) [0..4]

-- Limit to 5-bit words because that is all we're testing
prop_pack5_inverse = forAll (choose (0, 2^4)) $ \word -> 
  all (\n -> Base32.unpack5 n (Base32.pack5 n word 0) == word) [0..7]

prop_base16_inverse :: B8.ByteString -> Bool
prop_base16_inverse bs =
  either (const False) (bs==)
    $ Base16.decode $ Base16.encode bs

return []
runTests = $quickCheckAll

main :: IO ()
main = void runTests
