module AOC10_1 where

import Data.List (sort)
import Data.Maybe (listToMaybe)

solve :: String -> Int
solve input = countElem 1 diffList * (countElem 3 diffList + 1)
  where 
    diffList = diffs 0 . sort . map read . lines $ input

countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (== i)

diffs :: Int -> [Int] -> [Int]
diffs _ [] = []
diffs n (x:xs) = (x - n):(diffs x xs)