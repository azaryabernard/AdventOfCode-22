module Day04 (day04) where

import Data.List.Split (splitOn)
import Data.List (isInfixOf, intersect)

mapIsContain :: [[[Int]]] -> [Bool]
mapIsContain = map $ \xs -> head xs `isInfixOf` last xs || last xs `isInfixOf` head xs

mapIsOverlap :: [[[Int]]] -> [Bool]
mapIsOverlap = map $ \xs -> not . null $ head xs `intersect` last xs

-- main, assuming input is valid. Yes, I love one liners. :)
day04 :: IO ()
day04 = do 
  input <- map (splitOn ",") . lines <$> readFile "input.data"
  let parsed = (map.map) (\xs-> let ys = splitOn "-" xs in [read.head $ ys .. read.last $ ys]) input
  -- parsed :: [[[Int]]] = [[[14..38], [14..14]], [[2..10], [3..55]], ...]
  putStrLn $ (++) "Total assignment pairs of one range fully contains the other: " 
           $ show . length . filter id . mapIsContain $ parsed
  putStrLn $ (++) "Total assignment pairs that overlap at all: " 
           $ show . length . filter id . mapIsOverlap $ parsed
