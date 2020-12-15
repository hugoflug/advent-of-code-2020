module AOC9_2 where

import qualified Data.Sequence as S
import Data.Sequence (Seq(..), take, drop, (|>), (<|), index)
import Data.Ix (range)

solve :: String -> Int
solve input = (\l -> minimum l + maximum l) $ findContiguousSum (findNonSum 25 list) list
  where
    list = S.fromList . map read . lines $ input

slice :: Int -> Int -> Seq a -> Seq a
slice from to = S.take (to - from + 1) . S.drop from

findContiguousSum :: Int -> Seq Int -> Seq Int
findContiguousSum n seq = head . filter (\s -> sum s == n) $ do
    x <- range (0, S.length seq - 1)
    y <- range (0, S.length seq - 1)
    return $ slice x y seq

findNonSum :: Int -> Seq Int -> Int
findNonSum n seq = findNonSum' (S.take n seq) (S.drop n seq)

findNonSum' :: Seq Int -> Seq Int -> Int
findNonSum' pre (x :<| xs) =
  if hasSum x pre then findNonSum' (S.drop 1 pre |> x) xs
  else x

hasSum :: Int -> Seq Int -> Bool
hasSum n seq = 
  any (== n) $ do
    x <- seq
    y <- seq
    return $ x + y
