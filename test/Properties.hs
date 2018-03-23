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
    $ Base64.decode $ Base64.encode bs

prop_base16_inverse :: B8.ByteString -> Bool
prop_base16_inverse bs =
  either (const False) (bs==)
    $ Base16.decode $ Base16.encode bs

prop_base32_inverse :: B8.ByteString -> Bool
prop_base32_inverse bs =
  either (const False) (bs==)
    $ Base32.decode $ Base32.encode bs

return []
runTests = $quickCheckAll

main :: IO ()
main = void runTests
