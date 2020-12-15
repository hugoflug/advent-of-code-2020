module AOC11_2 where

import Data.Matrix (Matrix, fromLists, getElem, mapPos, ncols, nrows, safeGet)
import Data.Maybe (fromMaybe)

solve :: String -> Int
solve = count '#' . untilStable updateMatrix . fromLists . lines

count :: (Foldable f, Eq a) => a -> f a -> Int
count elem mx =
  foldr
    (\e count ->
       if e == elem
         then count + 1
         else count)
    0
    mx

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f v =
  if v == f v
    then v
    else untilStable f (f v)

updateMatrix :: Matrix Char -> Matrix Char
updateMatrix mx = mapPos (updateCell mx) mx

updateCell :: Matrix Char -> (Int, Int) -> Char -> Char
updateCell mx pos v
  | v == 'L' && visible '#' mx pos == 0 = '#'
  | v == '#' && visible '#' mx pos >= 5 = 'L'
  | otherwise = v

countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (== i)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

firstHit :: Matrix Char -> [(Int, Int)] -> Maybe Char
firstHit mx =
  safeHead . filter (/= '.') . map (\(r, c) -> getElem r c mx)

paths :: Matrix a -> (Int, Int) -> [[(Int, Int)]] 
paths (row, col) = [up, upright, right, downright, down, downleft, left, upleft]
  where
    up = [(r, col) | r <- [1..row - 1]]
    upright = zip [1..row - 1] [col + 1..ncols mx]
    right = [(row, c) | c <- [col+1..ncols mx]]
    downright =  zip [row + 1..nrows mx] [col + 1..ncols mx]
    down = [(r, col) | r <- [row + 1..nrows mx]]
    downleft =zip [row + 1..nrows mx] [col + 1..ncols mx]


up :: (Int, Int) -> [(Int, Int)]
up (row, col) = [(r, col) | r <- [1..row - 1]]

visible :: Char -> Matrix Char -> (Int, Int) -> Int
visible v mx = countElem (Just '#') . map (firstHit mx) . paths mx
