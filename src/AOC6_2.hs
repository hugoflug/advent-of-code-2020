module AOC6_2 where

import Data.List.Split (splitOn)
import Data.Set (fromList, size, Set, intersection)
import Data.List (foldl1')

solve :: String -> Int
solve = sum . map count . splitOn "\n\n"

count :: String -> Int 
count = size . foldl1' intersection . map fromList . lines
