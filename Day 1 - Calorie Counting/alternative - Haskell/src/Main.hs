module Main (main) where

import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  xs <- take 3 . sortOn negate . map (sum . catMaybes) . 
   splitOn [Nothing] . map (\x-> readMaybe x :: Maybe Integer) . lines <$> readFile "src/input.data"
  putStrLn $ "Maximum Calorie Content: " ++ (show . head) xs 
  putStrLn $ "Sum of top 3 "++ show xs ++": " ++ (show . sum) xs