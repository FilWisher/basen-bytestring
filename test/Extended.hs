import qualified Data.ByteString.Internal.Extended as Internal
import qualified Data.ByteString.Char8  as B8
import Test.QuickCheck

lol :: String
lol = "hi"

encoding = Internal.mkEnc Internal.table Nothing

prop_inverse = forAll (choose (0, B8.length Internal.table-1)) $ \n ->
  case Internal.decodeWord encoding (Internal.encodeWord encoding $ fromIntegral n) of
    Nothing -> False
    Just v -> v == fromIntegral n

main :: IO ()
main = quickCheck prop_inverse
