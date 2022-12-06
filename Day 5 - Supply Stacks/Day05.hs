module Day05 (run) where

import Data.List.Split (splitOn, chunksOf, splitPlacesBlanks)
import Data.List (transpose)
import Data.Char (isAlpha)

popB :: Bool -> Int -> [a] -> ([a], [a])
popB block n xs = let (ys, zs) = splitAt n xs in ((if block then id else reverse) ys, zs)

moveStack :: Bool -> [String] -> [Int] -> [String]
moveStack block stacks [n, from, to] = s1 ++ s2':s3 ++ s4':s5
  where (s2', s4') 
         | from <= to = let (p, r) = popB block n $ head s2 in (r, p ++ head s4)
         | otherwise = let (p, r) = popB block n $ head s4 in (p ++ head s2, r)
        (s1, s2, s3, s4, s5) = (head ss, ss!!1, ss!!2, ss!!3, last ss)
        ss = let (f, t) = (min from to, max from to) 
             in splitPlacesBlanks [f-1, 1, t-f-1, 1, length stacks-t] stacks
moveStack _ stacks _ = stacks -- omit invalid moves

-- main, assuming input is valid.
run :: IO ()
run = do 
  input <- map lines . splitOn "\n\n" <$> readFile "input.data"
  let (stacks, moves) = (
          filter (not . null) . map (filter isAlpha) . transpose . head $ input,
          map (map ((\x -> read x :: Int) . last) . chunksOf 2 . words) . last $ input
        )
  putStrLn "Initial Stacks:"        >> print stacks
  putStrLn "Finished Stacks:"       >> print (foldl (moveStack False) stacks moves)
  putStrLn "Finished Block Stacks:" >> print (foldl (moveStack True) stacks moves)
