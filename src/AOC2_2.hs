module AOC2_2(solve) where

import Text.Regex.TDFA ((=~))
import Data.Char (digitToInt)

solve :: String -> Int
solve = length . filter valid . lines

valid :: String -> Bool
valid password = validPassword (read low) (read high) (head c) s
  where 
    (_, _, _, [low, high, c, s]) = 
       password =~ "([0-9]+)-([0-9]+) (.): (.+)" :: (String, String, String, [String])

xor :: Bool -> Bool -> Bool
x `xor` y = (x || y) && not (x && y)

validPassword :: Int -> Int -> Char -> String -> Bool
validPassword a b char str = (str !! (a - 1) == char) `xor` (str !! (b - 1) == char)