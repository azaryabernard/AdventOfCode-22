module Day08 (run) where

import Data.Char(digitToInt)
import Data.Bifunctor(bimap, second)

column :: Int -> [[a]] -> [a]
column n = map (!! max 0 n)

row :: Int -> [[a]] -> [a]
row n = (!! max 0 n)

apply :: (Int -> [Int] -> t) -> (t -> t -> t) -> [[Int]] -> [t]
apply f g xss = [f j (row i xss) `g` f i (column j xss) | 
                 i <- [0..length xss-1], j <- [0..length (head xss)-1]]

isVisibleAt :: Int -> [Int] -> Bool
isVisibleAt i xs = all (<xs!!i) left || all (<xs!!i) right
  where (left, right) = second (drop 1) $ splitAt i xs

countScoreAt :: Int -> [Int] -> Int
countScoreAt i xs = len left * len right
  where (left, right) = bimap reverse (drop 1) $ splitAt i xs
        len xs' = (\l -> if l==length xs' then l else l+1) . length $ takeWhile (<xs!!i) xs'

countVisible :: [[Int]] -> Int -- Part 1
countVisible = length . filter id . apply isVisibleAt (||)

highestScore :: [[Int]] -> Int -- Part 2
highestScore = maximum . apply countScoreAt (*)

-- main, assuming input is valid.
run :: IO ()
run = do 
  input <- (map . map) digitToInt . lines <$> readFile "input.data"
  putStr "Visible trees from outside the grid: " >> print (countVisible input)
  putStr "Highest tree's scenic score: " >> print (highestScore input)
