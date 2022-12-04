module Day04 (day04) where

import Data.List.Split (splitOn)
import Data.List (isInfixOf, intersect)

filterContain :: [[[Int]]] -> [[[Int]]]
filterContain = filter $ \xs -> head xs `isInfixOf` last xs || last xs `isInfixOf` head xs

filterOverlap :: [[[Int]]] -> [[[Int]]]
filterOverlap = filter $ \xs -> not . null $ head xs `intersect` last xs

-- main, assuming input is valid. Yes, I love one liners. :)
day04 :: IO ()
day04 = do 
  input <- map (splitOn ",") . lines <$> readFile "input.data"
  let parsed = map.map $ \xs -> let ys = splitOn "-" xs in [read.head $ ys .. read.last $ ys]
  -- parsed :: [[[Int]]] = [[[14..38], [14..14]], [[2..10], [3..55]], ...]
  putStrLn $ (++) "Total assignment pairs of one range fully contains the other: " 
           $ show . length . filterContain . parsed $ input
  putStrLn $ (++) "Total assignment pairs that overlap at all: " 
           $ show . length . filterOverlap . parsed $ input
