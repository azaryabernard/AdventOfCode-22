module Day01 (day01) where

import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Data.List.Split (splitOn)

day01 :: IO ()
day01 = do
  xs <- take 3 . sortOn negate . map (sum . catMaybes) . 
       splitOn [Nothing] . map (\x-> readMaybe x :: Maybe Integer) . lines <$>  readFile "input.data"
  putStrLn $ "Maximum Calorie Content: " ++ (show . head) xs 
  putStrLn $ "Sum of top 3 "++ show xs ++ ": " ++ (show . sum) xs