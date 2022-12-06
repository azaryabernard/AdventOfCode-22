module Day05 (run) where

import Data.List.Split (splitOn, chunksOf, splitPlacesBlanks)
import Data.List (transpose)
import Data.Tuple (swap)
import Data.Char (isAlpha)

popB :: Bool -> Int -> [a] -> ([a], [a])
popB block n xs = let (ys, zs) = splitAt n xs in ((if block then id else reverse) ys, zs)

moveStack :: Bool -> [String] -> [Int] -> [String]
moveStack block stacks [n, from, to] = s1 ++ s2' ++ s3 ++ s4' ++ s5
  where
    (s2', s4') = (if from<=to then id else swap) ([rest], [popped ++ head s4])
    (popped, rest) = popB block n . head $ s2
    (s1, s2, s3, s4, s5) 
      | from<=to = let xs = split from to in (head xs, xs!!1, xs!!2, xs!!3, last xs)
      | otherwise = let xs = split to from in (head xs, xs!!3, xs!!2, xs!!1, last xs)
    split from' to' = splitPlacesBlanks [from'-1, 1, to'-from'-1, 1, length stacks - to'] stacks
moveStack _ stacks _ = stacks -- omit invalid moves

-- main, assuming input is valid. Yes, I love one liners. :)
run :: IO ()
run = do 
  input <- map lines . splitOn "\n\n" <$> readFile "input.data"
  let (stacks, moves) = (
          filter (not . null) . map (filter isAlpha) . transpose . head $ input, 
          map (map ((\x -> read x :: Int) . last) . chunksOf 2 . words) . last $ input
        )
  putStrLn $ (++) "Initial Stacks: \n" $ show stacks
  putStrLn $ (++) "Finished Stacks: \n" $ show . foldl (moveStack False) stacks $ moves
  putStrLn $ (++) "Finished Block Stacks: \n" $ show . foldl (moveStack True) stacks $ moves
