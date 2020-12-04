module AOC4_1 where

import Data.List.Split (splitOn, splitOneOf)

solve :: String -> Int
solve = length . filter valid . parsePassports

type Passport = [(String, String)]

parsePassports :: String -> [Passport]
parsePassports = map parsePassport . splitOn "\n\n"

parsePassport :: String -> Passport
parsePassport = map ((\[a, b] -> (a, b)) . splitOn ":") . splitOneOf "\n "

mandatoryFields :: [String]
mandatoryFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

valid :: Passport -> Bool
valid passport = all (`elem` map fst passport) mandatoryFields