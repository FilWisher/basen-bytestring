import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as B8
import Test.QuickCheck

lol :: String
lol = "hi"

instance Arbitrary B8.ByteString where
  arbitrary = B8.pack <$> arbitrary

prop_inverse :: B8.ByteString -> Bool
prop_inverse bs =
  case Base16.encode bs >>= Base16.decode of
    Left _  -> False
    Right v -> bs == v

main :: IO ()
main = quickCheck prop_inverse
