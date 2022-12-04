module Day03 (run) where

import Data.List (intersect)
import Data.List.Split (chunksOf)
import Data.Char (ord, isUpper, isAlpha)

-- Calculate the priority of an item type
eval :: Char -> Int
eval c | isAlpha c = ord c - if isUpper c then 38 else 96 | otherwise = 0

priority :: [String] -> Int
priority = sum . map (eval . head . \xs -> let half = length xs`div`2 in intersect (take half xs) (drop half xs))

badge :: [String] -> Int
badge = sum . map (eval . head . foldr1 intersect) . chunksOf 3

-- main, assuming input is valid
run :: IO ()
run = do
  input <- lines <$> readFile "input.data"
  putStrLn $ "Sum of the priorities of all item types: " ++ show (priority input)
  putStrLn $ "Sum of the priorities of the team badges: " ++ show (badge input)