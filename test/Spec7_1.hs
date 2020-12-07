module Spec7_1 where

import AOC7_1
import Test.Hspec
import qualified Data.Set as S

test = do
  specify "parse" $ do
    parse "muted white bags contain 4 dark orange bags, 3 bright white bags." `shouldBe` ("muted white",  ["dark orange", "bright white"])
    parse "mirrored gray bags contain 4 mirrored turquoise bags." `shouldBe` ("mirrored gray", ["mirrored turquoise"])