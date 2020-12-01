module AOC1_2(solve) where

solve :: String -> Int
solve = (\l -> head $ [x * y * z | x <- l, y <- l, z <- l, x + y + z == 2020]) . map read . lines
