module AOC9_1 where

import qualified Data.Sequence as S
import Data.Sequence (Seq(..), take, drop, (|>))

solve :: String -> Int
solve = findNonSum 25 . S.fromList . map read . lines

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
