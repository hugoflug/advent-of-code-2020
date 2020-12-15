module AOC11_1 where

import Data.Matrix (Matrix, fromLists, getElem, ncols, nrows, safeGet, mapPos)
import Data.Maybe (fromMaybe)

solve :: String -> Int
solve = count '#' . untilStable updateMatrix . fromLists . lines

count :: (Foldable f, Eq a) => a -> f a -> Int
count elem mx = 
  foldr (\e count -> if e == elem then count + 1 else count) 0 mx

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f v = if v == f v then v else untilStable f (f v)

updateMatrix :: Matrix Char -> Matrix Char
updateMatrix mx = mapPos (updateCell mx) mx

updateCell :: Matrix Char -> (Int, Int) -> Char -> Char
updateCell mx pos v
  | v == 'L' && adjacents pos '#' mx == 0 = '#'
  | v == '#' && adjacents pos '#' mx >= 4 = 'L'
  | otherwise = v

countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (== i)

adjacents :: Eq a => (Int, Int) -> a -> Matrix a -> Int
adjacents (row, col) v mx = countElem True [matches r c | r <- adjPos row, c <- adjPos col]
  where 
    adjPos p = [p - 1, p, p + 1]
    matches r c = safeGet r c mx == Just v && not (r == row && c == col)
