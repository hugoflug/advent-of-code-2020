module Spec10_2 where

import Test.Hspec
import AOC10_2

import Data.List (sort)

test = do
  specify "variations" $ do 
    --variations [1] `shouldBe` 1
    variations [1, 2] `shouldBe` 1
    variations [1, 2, 3, 4] `shouldBe` 4
    variations [1, 2, 4, 7] `shouldBe` 2