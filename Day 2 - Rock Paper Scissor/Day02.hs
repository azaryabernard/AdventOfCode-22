module Day02 (day02) where

-- A: Rock, B: Paper, C: Scissors | A beats C, B beats A, C beats B
-- m: (0 -> X: Rock, Y: Paper, Z: Scissors), (1 -> X: Lose, Y: Draw, Z: Win)
-- Type  = (Rock) 1, (Paper) 2, (Scissors) 3
-- Eval  = (Win) 6, (Draw) 3, (Lose) 0
-- Score = Type + Eval
eval :: Int -> [Char] -> Int
eval m xs 
 | m `notElem` [0, 1] = 0
 | otherwise = case (head xs, last xs) of 
  ('A','X') -> [4, 3]!!m
  ('A','Y') -> [8, 4]!!m
  ('A','Z') -> [3, 8]!!m
  ('B','X') -> [1, 1]!!m
  ('B','Y') -> [5, 5]!!m
  ('B','Z') -> [9, 9]!!m
  ('C','X') -> [7, 2]!!m
  ('C','Y') -> [2, 6]!!m
  ('C','Z') -> [6, 7]!!m
  (_, _)    -> 0

-- main, assuming input is valid
day02 :: IO ()
day02 = do
  pairs (eval 0) >>= putStrLn . (++) "Score from strategy: "
  pairs (eval 1) >>= putStrLn . (++) "Score from top secret strategy: "
  where pairs f = do
         show . sum . map f . lines <$> readFile "input.data"
