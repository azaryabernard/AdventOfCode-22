module Day10 (run) where

import Data.List.Split (chunksOf)

data Operand = NOOP | ADDX deriving (Show, Eq, Enum)
data Instruction = Instruction Operand Int deriving (Show, Eq)

instance Read Instruction where
  readsPrec _ ('n':'o':'o':'p':_) = [(Instruction NOOP 0, "")]
  readsPrec _ ('a':'d':'d':'x':xs) = [(Instruction ADDX (read xs), "")]
  readsPrec _ _ = []

-- add a NOOP instruction before each ADDX instruction to increase cycle count
interpolate :: [Instruction] -> [Instruction]
interpolate [] = []
interpolate (Instruction op v : xs) = case op of
  NOOP -> Instruction op v : interpolate xs
  ADDX -> Instruction NOOP 0 : Instruction op v : interpolate xs

-- run the instructions, return the x values at each cycle
runInstructions :: [Instruction] -> [Int]
runInstructions = scanl (\acc (Instruction op v) -> case op of
  NOOP -> acc
  ADDX -> acc + v) 1 . interpolate

-- draw the CRT display
drawCRT :: [Int] -> String
drawCRT xs =  unlines $ chunksOf 80 crt
  where crt = concat [if inRange (i, xs!!i) then "██" else "  " | i <- [0..length xs-1]]
        inRange (i, x) = (i`mod`40) `elem` [(x-1)..(x+1)]

-- main, assuming input is valid.
run :: IO ()
run = do 
  input <- map (\x -> read x :: Instruction) . lines <$> readFile "input.data"
  let xs = runInstructions input
  let part1  = sum [i * xs!!(i-1) | i <- [20, 60, 100, 140, 180, 220]]
  let part2 = drawCRT $ init xs
  putStr "The sum of the six signal strengths: " >> print part1
  putStrLn ('\n':replicate 32 '-' ++ "[CRT Display]" ++ replicate 33 '-') >> putStrLn part2