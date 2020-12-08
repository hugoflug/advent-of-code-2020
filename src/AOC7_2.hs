module AOC7_2 where

import Control.Monad ((<=<), join)
import Data.List (uncons)
import Data.List.Split (splitOn, splitOneOf)
import qualified Data.Map as M
import Data.Map (Map, (!), lookup, findWithDefault)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set, union)
import qualified Data.Set as S
import Text.Regex.TDFA ((=~))

type Bag = String

solve :: String -> Int
solve = containing "shiny gold" . M.fromList . map parse . lines

containing :: Ord a => a -> Map a [(Int, a)] -> Int
containing c childMap = 
  sum . map (\(n, child) -> n + n * (containing child childMap)) $ findWithDefault [] c childMap

groups :: String -> String -> [String]
groups regex str = groups
  where
    (_, _, _, groups) = str =~ regex :: (String, String, String, [String])

parse :: String -> (Bag, [(Int, Bag)])
parse = (\[a, b] -> (a, parseContain b)) . splitOn " bags contain "
  where
    parseContain = 
      map (\[n, b] -> (read n, b)) 
        . filter (not . null) 
        . map (groups "([0-9]) (.*) bags?") 
        . splitOn "," 
        . init
    
