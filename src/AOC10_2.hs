module AOC10_2 where

import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Function.Memoize (memoize)

solve :: String -> Int
solve = memoize variations . (0 :) . sort . map read . lines

variations :: [Int] -> Int
variations = memoize variations'
  where
    variations' [] = 0
    variations' [_] = 1
    variations' (x:xs) = sum . map variations . startsWith (<= x + 3) $ xs

startsWith :: (a -> Bool) -> [a] -> [[a]]
startsWith pred [] = []
startsWith pred (x:xs) =
  if pred x then (x:xs):(startsWith pred xs)
  else startsWith pred xs
