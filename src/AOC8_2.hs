module AOC8_2 where

import Data.Sequence(Seq(..), fromList, index)
import Data.List.Split (splitOn)
import Data.Set (Set, member, insert, empty, size)

type Accumulator = Int
type ProgramCounter = Int
type ProgramState = (Accumulator, ProgramCounter)
type Instruction = (String, Int)
type Program = Seq Instruction

solve :: String -> Int
solve = run . fromList . map parse . lines

run :: Program -> Maybe Int
run = run' empty (0, 0)

run' :: Set Int -> ProgramState -> Program -> Maybe Int
run' visited state@(acc, pc) program
  | pc == size program = acc
  | pc `member` visited = Nothing
  | otherwise = run' (insert pc visited) (interpret program state) program

interpret :: Program -> ProgramState -> ProgramState
interpret program (acc, pc) =
  case program `index` pc of 
    ("nop", _) -> (acc, pc + 1)
    ("acc", n) -> (acc + n, pc + 1)
    ("jmp", n) -> (acc, pc + n)

parse :: String -> Instruction
parse = (\[a, b] -> (a, readN b)) . splitOn " "

readN :: String -> Int
readN ('+':xs) = read xs
readN x = read x