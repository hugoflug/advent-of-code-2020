import Test.Hspec
import qualified AOC1_1
import qualified AOC1_2
import qualified AOC2_1
import qualified AOC2_2
import qualified AOC3_1
import qualified AOC3_2

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
  describe "2_1" $ do
    withInput $ do
      input <- readFile "test/AOC2.txt"
      AOC2_1.solve input `shouldBe` 582
  describe "2_2" $ do
    withInput $ do
      input <- readFile "test/AOC2.txt"
      AOC2_2.solve input `shouldBe` 729
  describe "3_1" $ do
    withInput $ do
      input <- readFile "test/AOC3.txt"
      AOC3_1.solve input `shouldBe` 274
  describe "3_2" $ do
    withInput $ do
      input <- readFile "test/AOC3.txt"
      AOC3_2.solve input `shouldBe` 6050183040