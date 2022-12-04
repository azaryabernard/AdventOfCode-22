module Main (main) where

-- All the days are imported here
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
-- import Day05 (day05)
-- import Day06 (day06)
-- import Day07 (day07)
-- import Day08 (day08)
-- import Day09 (day09)
-- import Day10 (day10)
-- import Day11 (day11)
-- import Day12 (day12)
-- import Day13 (day13)
-- import Day14 (day14)
-- import Day15 (day15)
-- import Day16 (day16)
-- import Day17 (day17)
-- import Day18 (day18)
-- import Day19 (day19)
-- import Day20 (day20)
-- import Day21 (day21)
-- import Day22 (day22)
-- import Day23 (day23)
-- import Day24 (day24)
-- import Day25 (day25)

-- Function Imports
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Control.Monad (void)
import System.Directory (getCurrentDirectory, setCurrentDirectory)

challenges :: [(String, IO ())]
challenges = [
  ("Day 1 - Calorie Counting", day01),
  ("Day 2 - Rock Paper Scissor", day02),
  ("Day 3 - Rucksack Reorganization", day03),
  ("Day 4 -", day04)
 ]

runChallenge :: Int -> IO()
runChallenge n | n > length challenges = putStrLn "Invalid challenge number!"
runChallenge n = do
  let (challenge, action) = challenges !! (n-1)
  putStrLn $ "Running: " ++ challenge ++ "\n" ++ replicate 50 '-'
  root <- getCurrentDirectory
  setCurrentDirectory challenge >> action
  setCurrentDirectory root
  putStrLn $ replicate 50 '-'

main :: IO ()
main = do
  putStrLn "Enter the challenge number you want to run (1-25) or nothing to quit:"
  input <- filter (`elem` [1..25]) . mapMaybe readMaybe . words <$> getLine
  if null input
    then  void $ putStrLn "Bye!"
    else runChallenge (head input) >> main
