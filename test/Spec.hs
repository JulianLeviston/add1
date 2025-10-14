import Test.Hspec
import NumberStoreSpec (numberStoreSpec)
import LibSpec (libSpec)

main :: IO ()
main = hspec $ do
  numberStoreSpec
  libSpec