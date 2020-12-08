module Spec8_1 where

import AOC8_1
import Test.Hspec
import qualified Data.Set as S

test = do
  specify "parse" $ do
    parse "jmp -422" `shouldBe` ("jmp", -422)
    parse "acc +1" `shouldBe` ("acc", 1)
  specify "solve" $ do
    input <- readFile "test/AOC8_1.txt"
    solve input `shouldBe` 5
 