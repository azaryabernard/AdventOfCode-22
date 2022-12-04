module Day03 (day03) where

import Data.List (intersect)
import Data.List.Split (chunksOf)
import Data.Char (ord, isUpper)

-- | Calculate the priority of an item type
eval :: Char -> Int
eval c = ord c - if isUpper c then 38 else 96

-- main, assuming input is valid
day03 :: IO ()
day03 = do
  input <- lines <$> readFile "input.data"
  let prio = sum $ eval . head . (\xs -> let half = (length xs`div`2) in intersect (take half xs) (drop half xs)) <$> input
  let badge = sum $ eval . head . foldr1 intersect <$> chunksOf 3 input
  putStrLn $ "Sum of the priorities of all item types: " ++ show prio
  putStrLn $ "Sum of the priorities of the team badges: " ++ show badge
