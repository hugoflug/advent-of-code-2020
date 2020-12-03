module AOC3_1(solve) where

import Data.Matrix (Matrix, fromLists, getElem, ncols, nrows)

solve :: String -> Int
solve = trees 1 3 . fromLists . lines

trees :: Int -> Int -> Matrix Char -> Int
trees down right = length . filter (== '#') . squares down right

squares :: Int -> Int -> Matrix Char -> [Char]
squares down right mx = 
  [elemAt row col mx | (row, col) <- zip [0, down..nrows mx - 1] [0, right..]]

elemAt :: Int -> Int -> Matrix Char -> Char
elemAt row col mx = getElem (row + 1) ((col `mod` ncols mx) + 1) mx 