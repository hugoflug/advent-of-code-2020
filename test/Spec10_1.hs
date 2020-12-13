module Spec10_1 where

import Test.Hspec
import AOC10_1

import Data.List (sort)

test = do
  specify "diffs" $ do 
    diffs 0 [1, 2] `shouldBe` [1, 1]
    diffs 0 [1, 2, 4, 7, 8, 9] `shouldBe` [1, 1, 2, 3, 1, 1]
