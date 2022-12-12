module Main (main) where

import Day01 (run)
import Day02 (run)
import Day03 (run)
import Day04 (run)
import Day05 (run)
import Day06 (run)
import Day07 (run)
import Day08 (run)
import Day09 (run)
import Day10 (run)
-- import Day11 (run)
-- import Day12 (run)
-- import Day13 (run)
-- import Day14 (run)
-- import Day15 (run)
-- import Day16 (run)
-- import Day17 (run)
-- import Day18 (run)
-- import Day19 (run)
-- import Day20 (run)
-- import Day21 (run)
-- import Day22 (run)
-- import Day23 (run)
-- import Day24 (run)
-- import Day25 (run)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import System.Directory (
  getCurrentDirectory, 
  setCurrentDirectory, 
  getDirectoryContents
  )

-- List of all the challenges
challenges :: [IO ()]
challenges = [
  Day01.run, Day02.run, Day03.run, Day04.run, Day05.run,
  Day06.run, Day07.run, Day08.run, Day09.run, Day10.run
  -- Day11.run, Day12.run, Day13.run, Day14.run, Day15.run,
  -- Day16.run, Day17.run, Day18.run, Day19.run, Day20.run,
  -- Day21.run, Day22.run, Day23.run, Day24.run, Day25.run
 ]

findChallengeDir :: Int -> IO String
findChallengeDir n = do 
  contents <- getCurrentDirectory >>= getDirectoryContents
  -- filter out all non directories and the ones that don't start with "Day"
  let dirs = filter (\x -> x /= "." && x /= "..") contents
  let days = filter (\x -> take 3 x == "Day") dirs
  -- filter out all directories that don't have the correct number
  let day = filter (\x -> let xs = words x in not (null xs) && read (xs !! 1) == n) days
  if null day then return "" else return $ head day

runChallenge :: Int -> IO()
runChallenge n | n > length challenges = putStrLn "Invalid challenge number!"
runChallenge n = do
  let (action, challengeDir) = (challenges!!(n-1), findChallengeDir n)
  challenge <- challengeDir
  putStrLn $ "Running: " ++ challenge ++ "\n" ++ replicate 70 '-'
  root <- getCurrentDirectory
  setCurrentDirectory challenge >> action
  setCurrentDirectory root
  putStrLn $ replicate 70 '-'

-- Main function for all the challenges in Advent of Code 2022 project
main :: IO ()
main = do
  putStrLn "Enter the challenge number you want to run (1-25) or nothing to quit:"
  input <- filter (`elem` [1..25]) . mapMaybe readMaybe . words <$> getLine
  if null input
    then putStrLn "Bye!"
    else runChallenge (head input) >> main
