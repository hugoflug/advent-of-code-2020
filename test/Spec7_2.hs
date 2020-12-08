module Spec7_2 where

import AOC7_2
import Test.Hspec
import qualified Data.Set as S

test = do
  specify "parse" $ do
    parse "clear gold bags contain no other bags." `shouldBe` ("clear gold",  [])
    parse "posh gray bags contain 1 plaid crimson bag, 1 mirrored yellow bag." `shouldBe` ("posh gray", [(1, "plaid crimson"), (1, "mirrored yellow")])