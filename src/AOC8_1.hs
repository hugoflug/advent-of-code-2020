module AOC8_1 where

import Data.Sequence(Seq(..), fromList, index)
import Data.List.Split (splitOn)
import Data.Set (Set, member, insert, empty)

type Accumulator = Int
type ProgramCounter = Int
type ProgramState = (Accumulator, ProgramCounter)
type Instruction = (String, Int)
type Program = Seq Instruction

solve :: String -> Int
solve = findAccValue . fromList .  map parse . lines

findAccValue :: Program -> Int
findAccValue = findAccValue' empty (0, 0)

findAccValue' :: Set Int -> ProgramState -> Program -> Int
findAccValue' visited state@(acc, pc) program =
  if pc `member` visited then acc
  else findAccValue' (insert pc visited) (interpret program state) program

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