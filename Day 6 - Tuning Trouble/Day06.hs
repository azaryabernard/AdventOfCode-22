module Day06 (run) where

import Data.List (tails, findIndex)

isUnique :: Eq a => [a] -> Bool
isUnique [] = True
isUnique (x:xs) = x `notElem` xs && isUnique xs

rollingWindow :: Int -> [a] -> [[a]]
rollingWindow n = foldr (zipWith (:)) (repeat []) . take n . tails

firstNUnique :: Eq a => Int -> [a] -> Maybe Int
firstNUnique n = fmap (+n) . findIndex id . map isUnique . rollingWindow n

-- main, assuming input is valid.
run :: IO ()
run = do 
  input <- readFile "input.data"
  putStrLn "First index after 4 unique chars: "  >> print (firstNUnique 4 input)
  putStrLn "First index after 14 unique chars: " >> print (firstNUnique 14 input)
