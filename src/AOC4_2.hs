module AOC4_2 where

import Data.List.Split (splitOn, splitOneOf)
import Text.Regex.TDFA ((=~))

solve :: String -> Int
solve = length . filter valid . parsePassports

type Passport = [(String, String)]

parsePassports :: String -> [Passport]
parsePassports = map parsePassport . splitOn "\n\n"

parsePassport :: String -> Passport
parsePassport = map ((\[a, b] -> (a, b)) . splitOn ":") . splitOneOf "\n "

mandatoryFields :: [String]
mandatoryFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

hasMandatoryFields :: Passport -> Bool
hasMandatoryFields passport = all (`elem` map fst passport) mandatoryFields

validField :: (String, String) -> Bool
validField field =
  case field of
    ("byr", val) -> validByr val
    ("iyr", val) -> validIyr val
    ("eyr", val) -> validEyr val
    ("hgt", val) -> validHgt val
    ("hcl", val) -> validHcl val
    ("ecl", val) -> validEcl val
    ("pid", val) -> validPid val
    (_, _) -> True

valid :: Passport -> Bool
valid passport = hasMandatoryFields passport && all validField passport

validYear :: Int -> Int -> String -> Bool
validYear first last = (\i -> i >= first && i <= last) . read

validByr :: String -> Bool
validByr = validYear 1920 2002

validIyr :: String -> Bool
validIyr = validYear 2010 2020

validEyr :: String -> Bool
validEyr = validYear 2020 2030

groups :: String -> String -> [String]
groups regex str = groups
  where
    (_, _, _, groups) = str =~ regex :: (String, String, String, [String])

validHgt :: String -> Bool
validHgt hgt =
  case groups "^([0-9]+)(cm|in)$" hgt of
    [value, unit] -> validHeight (read value) unit
    _ -> False

validHeight :: Int -> String -> Bool
validHeight value unit =
  case (value, unit) of
    (inches, "in") -> inches >= 59 && inches <= 76
    (cm, "cm") -> cm >= 150 && cm <= 193
    _ -> False

validHcl :: String -> Bool
validHcl hcl = hcl =~ "^#([0-9]|[a-f]){6}$"

validEcl :: String -> Bool
validEcl ecl = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPid :: String -> Bool
validPid pid = pid =~ "^[0-9]{9}$"
