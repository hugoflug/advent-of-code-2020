module AOC1_1(solve) where

solve :: String -> Int
solve = (\l -> head $ [x * y | x <- l, y <- l, x + y == 2020]) . map read . lines
