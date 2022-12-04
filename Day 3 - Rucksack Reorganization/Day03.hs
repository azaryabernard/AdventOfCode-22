module Day03 (run) where

import Data.List (intersect)
import Data.List.Split (chunksOf)
import Data.Char (ord, isUpper, isAlpha)

-- Calculate the priority of an item type
eval :: Char -> Int
eval c | isAlpha c = ord c - if isUpper c then 38 else 96 | otherwise = 0

prio :: [String] -> Int
prio = sum . map (eval . head . \xs -> let half = length xs`div`2 in intersect (take half xs) (drop half xs))

badge :: [String] -> Int
badge input = sum $ eval . head . foldr1 intersect <$> chunksOf 3 input

-- main, assuming input is valid
run :: IO ()
run = do
  input <- lines <$> readFile "input.data"
  putStrLn $ "Sum of the priorities of all item types: " ++ show (prio input)
  putStrLn $ "Sum of the priorities of the team badges: " ++ show (badge input)