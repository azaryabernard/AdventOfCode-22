module Day09 (run) where

import Data.Bifunctor(bimap)
import Data.List (nub)

type Position = (Int, Int)
type Step = Int
data Direction = U | D | L | R | UL | UR | DL | DR deriving (Show, Eq, Enum, Read)

moveIndex :: Direction -> Position -> Position
moveIndex direction (x,y) = case direction of
  U -> (x, y-1);  UL -> (x-1, y-1)
  D -> (x, y+1);  UR -> (x+1, y-1)
  L -> (x-1, y);  DL -> (x-1, y+1)
  R -> (x+1, y);  DR -> (x+1, y+1)

findDirection :: Position -> Position -> Direction
findDirection (xt, yt) (xh, yh) 
  | xt < xh && yt < yh  = DR  
  | xt > xh && yt < yh = DL
  | xt < xh && yt > yh =  UR  
  | xt > xh && yt > yh = UL
  | yt < yh = D | yt > yh = U 
  | xt < xh = R | xt > xh = L
  | otherwise = error "findDirection: undefined"

nodeAroundNode :: Position -> Position -> Bool 
nodeAroundNode node (x, y) = or [node == (x+i, y+j) | i <- [-1, 0, 1], j <- [-1, 0, 1]]

-- to let the previous node follow the next node
followNext :: Position -> Position -> Position
followNext prevPos nextPos
  | nodeAroundNode prevPos nextPos = prevPos
  | otherwise = moveIndex (findDirection prevPos nextPos) prevPos

-- move the rope and all its tails, move head first, then the tails follow next
moveNodes :: Direction -> Step ->  [Position] -> [[Position]]
moveNodes _ 0 ps = [ps]
moveNodes direction len ps = ps : moveNodes direction (len-1) ps''
  where ps'' = scanl1 (flip followNext) ps' -- scan is fold that retains the process
        ps' = moveIndex direction (head ps) : tail ps

-- move the rope with all the steps in the directions, save the history
moveRope ::  [Position] -> [(Direction, Step)] -> [[Position]]
moveRope ps [] = [ps]
moveRope ps ((direction, len) : xs) = ys ++ moveRope ps' xs
  where ps' = if null ys then ps else last ys
        ys = moveNodes direction len ps

traceTail :: [[Position]] -> [Position]
traceTail = nub . map last -- distinct positions of the tail

prettyPrint :: [Position] -> IO () -- print the grid 😉
prettyPrint ps =  putStrLn "" >> mapM_ putStrLn fillGrid
  where fillGrid = [[if (x,y) `elem` ps then 'X' else '.' | x <- [minX..maxX]] | y <- [minY..maxY]]
        (minX, minY) = (minimum . map fst $ ps, minimum . map snd $ ps)
        (maxX, maxY) = (maximum . map fst $ ps, maximum . map snd $ ps)

-- main, assuming input is valid.
run :: IO ()
run = do 
  input <- map (bimap (\x->read x::Direction) (\x->read x::Step) . splitAt 1) . lines <$> readFile "input.data"
  let part1 = traceTail $ moveRope (replicate 2 (0,0)) input
  prettyPrint part1 >> putStrLn ("Visited Positions of the rope's tail: " ++ show (length part1))
  let part2 = traceTail $ moveRope (replicate 10 (0,0)) input
  prettyPrint part2 >> putStrLn ("Visited Positions of the rope and its extended tail: " ++ show (length part2))
