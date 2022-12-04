module Main (main) where

-- All the days are imported here
import Day01 (run)
import Day02 (run)
import Day03 (run)
import Day04 (run)
import Day05 (run)
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

challenges :: [(IO (), String)]
challenges = [
  (Day01.run, "Day 1 - Calorie Counting"),
  (Day02.run, "Day 2 - Rock Paper Scissor"),
  (Day03.run, "Day 3 - Rucksack Reorganization"),
  (Day04.run, "Day 4 - Camp Cleanup"),
  (Day05.run, "Day 5 - xxx")
 ]

runChallenge :: Int -> IO()
runChallenge n | n > length challenges = putStrLn "Invalid challenge number!"
runChallenge n = do
  let (action, challenge) = challenges !! (n-1)
  putStrLn $ "Running: " ++ challenge ++ "\n" ++ replicate 70 '-'
  root <- getCurrentDirectory
  setCurrentDirectory challenge >> action
  setCurrentDirectory root
  putStrLn $ replicate 70 '-'

main :: IO ()
main = do
  putStrLn "Enter the challenge number you want to run (1-25) or nothing to quit:"
  input <- filter (`elem` [1..25]) . mapMaybe readMaybe . words <$> getLine
  if null input
    then  void $ putStrLn "Bye!"
    else runChallenge (head input) >> main
