module Day01 (run) where

import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Data.List.Split (splitOn)

run :: IO ()
run = do
  maxOf3 <- take 3 . sortOn negate . map (sum . catMaybes) . splitOn [Nothing]
            . map (\x-> readMaybe x :: Maybe Integer) . lines <$> readFile "input.data"
  putStrLn $ "Maximum Calorie Content: " ++ show (head maxOf3)
  putStrLn $ "Sum of top 3 " ++ show maxOf3 ++ ": " ++ show (sum maxOf3)