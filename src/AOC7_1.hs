module AOC7_1 where

import Control.Monad (join, (<=<))
import Data.List (uncons)
import Data.List.Split (splitOn, splitOneOf)
import qualified Data.Map as M
import Data.Map (Map, (!), lookup)
import Data.Set (Set, union)
import qualified Data.Set as S
import Data.Maybe (fromJust, fromMaybe)

type Bag = String

solve :: String -> Int
solve = length . ancestors "shiny gold" . toMultiMap . (rev . parse <=< lines)

toMultiMap :: (Ord a, Ord b) => [(a, b)] -> Map a (Set b)
toMultiMap = M.fromListWith union . map (mapSnd S.singleton)

ancestors :: Ord a => a -> Map a (Set a) -> Set a
ancestors c parentMap = parents `union` ancestorsOf parents
  where
    ancestorsOf = S.unions . S.map (\x -> ancestors x parentMap)
    parents = fromMaybe S.empty $ M.lookup c parentMap

rev :: (a, [b]) -> [(b, a)]
rev (b, cs) = (\c -> (c, b)) <$> cs

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

slice :: Int -> Int -> [a] -> [a]
slice from to = take (to - from + 1) . drop from

parse :: String -> (Bag, [Bag])
parse = (\[a, b] -> (a, parseContain b)) . splitOn " bags contain "
  where
    parseContain = map (unwords . slice 1 2 . words) . splitOn "," . init
