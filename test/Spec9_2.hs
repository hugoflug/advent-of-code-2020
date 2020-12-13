module Spec9_2 where

import AOC9_2
import Test.Hspec
import qualified Data.Set as S
import Data.Sequence 

test = do
  specify "findContiguousSum" $ do
    findContiguousSum 6 (fromList [10, 8, 1, 2, 3, 7, 4]) `shouldBe` fromList [1, 2, 3]
    findContiguousSum 6 (fromList [10, 8, 3, 1, 2, 7, 4]) `shouldBe` fromList [3, 1, 2]
    findContiguousSum 6 (fromList [3, 1, 2, 10, 8, 7, 4]) `shouldBe` fromList [3, 1, 2]
    findContiguousSum 6 (fromList [1, 3, 1, 2, 7]) `shouldBe` fromList [3, 1, 2]
    findContiguousSum 6 (fromList [10, 8, 3, 1, 2]) `shouldBe` fromList [3, 1, 2]
