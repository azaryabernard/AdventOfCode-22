module Main (main) where

-- A = X: Rock, B = Y: Paper, C = Z: Scissors
-- Type  = (Rock) 1, (Paper) 2, (Scissors) 3
-- Eval  = (Win) 6, (Draw) 3, (Lose) 0
-- Score = Type + Eval
eval :: [Char] -> Int
eval xs = case (head xs, last xs) of 
  ('A','X') -> 4
  ('A','Y') -> 8
  ('A','Z') -> 3
  ('B','X') -> 1
  ('B','Y') -> 5
  ('B','Z') -> 9
  ('C','X') -> 7
  ('C','Y') -> 2
  ('C','Z') -> 6
  (_, _)    -> 0

-- X: Lose, Y: Draw, Z: Win
eval' :: [Char] -> Int
eval' xs = case (head xs, last xs) of 
  ('A','X') -> 3
  ('A','Y') -> 4
  ('A','Z') -> 8
  ('B','X') -> 1
  ('B','Y') -> 5
  ('B','Z') -> 9
  ('C','X') -> 2
  ('C','Y') -> 6
  ('C','Z') -> 7
  (_, _)    -> 0

-- main, assuming input is valid
main :: IO ()
main = do
  pairs eval >>= putStrLn . (++) "Score from strategy: "
  pairs eval' >>= putStrLn . (++) "Score from top secret strategy: "
  where pairs f = do
         show . sum . map f . lines <$> readFile "src/input.data"