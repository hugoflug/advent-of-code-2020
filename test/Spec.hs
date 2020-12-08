import Test.Hspec
import qualified AOC1_1
import qualified AOC1_2
import qualified AOC2_1
import qualified AOC2_2
import qualified AOC3_1
import qualified AOC3_2
import qualified AOC4_1
import qualified AOC4_2
import qualified AOC5_1
import qualified AOC5_2
import qualified AOC6_1
import qualified AOC6_2
import qualified AOC7_1
import qualified AOC7_2
import qualified AOC8_1

import qualified Spec4_2
import qualified Spec5_1
import qualified Spec7_1
import qualified Spec7_2
import qualified Spec8_1

goldStar = specify "GOLD STAR *"

main :: IO ()
main = hspec $ do
  describe "1_1" $ do
    goldStar $ do
      input <- readFile "test/AOC1.txt"
      AOC1_1.solve input `shouldBe` 910539
  describe "1_2" $ do
    goldStar $ do
      input <- readFile "test/AOC1.txt"
      AOC1_2.solve input `shouldBe` 116724144
  describe "2_1" $ do
    goldStar $ do
      input <- readFile "test/AOC2.txt"
      AOC2_1.solve input `shouldBe` 582
  describe "2_2" $ do
    goldStar $ do
      input <- readFile "test/AOC2.txt"
      AOC2_2.solve input `shouldBe` 729
  describe "3_1" $ do
    goldStar $ do
      input <- readFile "test/AOC3.txt"
      AOC3_1.solve input `shouldBe` 274
  describe "3_2" $ do
    goldStar $ do
      input <- readFile "test/AOC3.txt"
      AOC3_2.solve input `shouldBe` 6050183040
  describe "4_1" $ do
    goldStar $ do
      input <- readFile "test/AOC4.txt"
      AOC4_1.solve input `shouldBe` 242
  describe "4_2" $ do
    goldStar $ do
      input <- readFile "test/AOC4.txt"
      AOC4_2.solve input `shouldBe` 186
    Spec4_2.test
  describe "5_1" $ do
    goldStar $ do
      input <- readFile "test/AOC5.txt"
      AOC5_1.solve input `shouldBe` 951
    Spec5_1.test
  describe "5_2" $ do
    goldStar $ do
      input <- readFile "test/AOC5.txt"
      AOC5_2.solve input `shouldBe` 653
  describe "6_1" $ do
    goldStar $ do
      input <- readFile "test/AOC6.txt"
      AOC6_1.solve input `shouldBe` 6680
  describe "6_2" $ do
    goldStar $ do
      input <- readFile "test/AOC6.txt"
      AOC6_2.solve input `shouldBe` 3117
  describe "7_1" $ do
    goldStar $ do
      input <- readFile "test/AOC7.txt"
      AOC7_1.solve input `shouldBe` 242
    Spec7_1.test
  describe "7_2" $ do
    goldStar $ do
      input <- readFile "test/AOC7.txt"
      AOC7_2.solve input `shouldBe` 176035
    Spec7_2.test
  describe "8_1" $ do
    goldStar $ do
      input <- readFile "test/AOC8.txt"
      AOC8_1.solve input `shouldBe` 0
    Spec8_1.test
