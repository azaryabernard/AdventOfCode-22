module Main (main) where

import Data.List (intersect)
import Data.List.Split (chunksOf)
import Data.Char (ord, isUpper)

-- | Calculate the priority of an item type
eval :: Char -> Int
eval c = ord c - if isUpper c then 38 else 96

-- main, assuming input is valid
main :: IO ()
main = do
  input <- lines <$> readFile "src/input.data"
  let prio = sum . map (eval . head . (\xs -> let half = (length xs `div` 2) in intersect (take half xs) (drop half xs))) $ input
  putStrLn $ "Sum of the priorities of all item types: " ++ show prio
  let badge = sum . map (eval . head . foldr1 intersect) . chunksOf 3 $ input
  putStrLn $ "Sum of the priorities of the team badges: " ++ show badge
