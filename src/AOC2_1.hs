module AOC2_1(solve) where

import Text.Regex.TDFA ((=~))

solve :: String -> Int
solve = length . filter valid . lines

valid :: String -> Bool
valid password = validPassword (read low) (read high) (head c) s
  where 
    (_, _, _, [low, high, c, s]) = 
       password =~ "([0-9]+)-([0-9]+) (.): (.+)" :: (String, String, String, [String])

validPassword :: Int -> Int -> Char -> String -> Bool
validPassword min max char str = occurs >= min && occurs <= max
  where occurs = length $ filter (== char) $ str