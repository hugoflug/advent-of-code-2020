module AOC8_2 where

import qualified Data.Sequence as Seq
import Data.Sequence(Seq(..), fromList, index, length, adjust)
import Data.List.Split (splitOn)
import Data.Set (Set, member, insert, empty, size)
import Data.Foldable (toList, foldl', msum)
import Data.Maybe (fromJust)

type Accumulator = Int
type ProgramCounter = Int
type ProgramState = (Accumulator, ProgramCounter)
type Instruction = (String, Int)
type Program = Seq Instruction

solve :: String -> Int
solve input = runAll $ replacements "jmp" "nop" program ++ replacements "nop" "jmp" program
  where 
    program = fromList . map parse . lines $ input

indices ::  String -> Program -> [Int]
indices instr = map fst . filter (\(ix, (i, _)) -> i == instr) . zip [0..] . toList

replacements :: String -> String -> Program -> [Program]
replacements origInstr replaceInstr program = 
  map (\ix -> adjust (\(instr, n) -> (replaceInstr, n)) ix program) . indices origInstr $ program

runAll :: [Program] -> Int
runAll = fromJust . msum . map run

run :: Program -> Maybe Int
run = run' empty (0, 0)

run' :: Set Int -> ProgramState -> Program -> Maybe Int
run' visited state@(acc, pc) program
  | pc == Seq.length program = Just acc
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