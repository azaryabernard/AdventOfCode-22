module Day04 (run) where

import Data.List.Split (splitOn)
import Data.List (isInfixOf, intersect)

filterContain :: [[[Int]]] -> [[[Int]]]
filterContain = filter $ \xs -> head xs `isInfixOf` last xs || last xs `isInfixOf` head xs

filterOverlap :: [[[Int]]] -> [[[Int]]]
filterOverlap = filter $ \xs -> not . null $ head xs `intersect` last xs

parse :: [String] -> [[[Int]]] -- [[[14..38], [14..14]], [[2..10], [3..55]], ...]
parse = map $ map (\xs -> let ys = splitOn "-" xs in [read.head $ ys .. read.last $ ys]) . splitOn ","

-- main, assuming input is valid. Yes, I love one liners. :)
run :: IO ()
run = do 
  input <- lines <$> readFile "input.data"
  putStrLn $ (++) "Total assignment pairs of one range fully contains the other: " 
           $ show . length . filterContain . parse $ input
  putStrLn $ (++) "Total assignment pairs that overlap at all: " 
           $ show . length . filterOverlap . parse $ input
