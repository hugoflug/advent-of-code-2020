module Spec5_1 where

import AOC5_1
import Test.Hspec

test = do
  specify "row" $ do
    row "B" `shouldBe` 64
    row "BB" `shouldBe` 96
    row "BBFFBBF" `shouldBe` 102
  specify "seatId" $ do
    seatId "BFFFBBFRRR" `shouldBe` 567
    seatId "FFFBBBFRRR" `shouldBe` 119
    seatId "BBFFBBFRLL" `shouldBe` 820
