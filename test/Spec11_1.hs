module Spec11_1 where

import AOC11_1
import Test.Hspec

import Data.Matrix (fromLists, Matrix)

test = do
  specify "adjacents" $ do
    let matrix = fromLists ([ ['#', '.', '#'], ['#', '.', '.'], ['#', '.', '.']])
    adjacents (2, 2) '#' matrix `shouldBe` 4
    adjacents (2, 2) '.' matrix `shouldBe` 4
    adjacents (1, 1) '#' matrix `shouldBe` 1
    adjacents (2, 1) '#' matrix `shouldBe` 2

