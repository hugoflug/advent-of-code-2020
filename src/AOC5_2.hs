module AOC5_2 where

import Data.Foldable (foldl')
import Data.List (sort)

solve :: String -> Int
solve = missingSeatId . sort . map seatId . lines

missingSeatId :: [Int] -> Int
missingSeatId list = findMissing (head list) list
  where 
    findMissing last (x:xs) =
      if x == last + 2 then last + 1
      else findMissing x xs

seatId :: String -> Int
seatId = (\(r, c) -> row r * 8 + column c) . splitAt 7

halfDiff :: Int -> Int -> Int
halfDiff high low = (high - low) `div` 2 + 1

parse :: Char -> Char -> Int -> String -> Int
parse lowChar highChar upperBound = fst . foldl' readChar (0, upperBound)
  where
    readChar (low, high) c
      | c == lowChar = (low, high - halfDiff high low) 
      | c == highChar = (low + halfDiff high low, high)

row :: String -> Int
row = parse 'F' 'B' 127

column :: String -> Int
column = parse 'L' 'R' 7