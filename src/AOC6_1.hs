module AOC6_1 where

import Control.Monad (join)
import Data.List.Split (splitOn)
import Data.Set (fromList, size)

solve :: String -> Int
solve = sum . map (size . fromList . join . lines) . splitOn "\n\n"