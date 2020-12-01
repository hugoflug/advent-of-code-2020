import Test.Hspec
import qualified AOC1_1
import qualified AOC1_2

withInput = it "GOLD STAR *"

main :: IO ()
main = hspec $ do
  describe "1_1" $ do
    withInput $ do
      input <- readFile "test/AOC1.txt"
      AOC1_1.solve input `shouldBe` 910539
  describe "1_2" $ do
    withInput $ do
      input <- readFile "test/AOC1.txt"
      AOC1_2.solve input `shouldBe` 116724144