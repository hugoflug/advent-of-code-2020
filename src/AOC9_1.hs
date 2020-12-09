module AOC9_1 where

import Data.Sequence (Seq)

solve :: String -> Int
solve = findNonSum 25 . map read . lines

findNonSum :: Int -> [Int] -> Int
findNonSum n list = findNonSum' (take n list) (drop n list)

findNonSum' :: [Int] -> [Int] -> Int
findNonSum' pre (x:xs) =
  if not $ hasSum x pre then x
  else findNonSum' (tail pre ++ [x]) xs

hasSum :: Int -> [Int] -> Bool
hasSum n list = not . null $ [x + y | x <- list, y <- list, x + y == n]
