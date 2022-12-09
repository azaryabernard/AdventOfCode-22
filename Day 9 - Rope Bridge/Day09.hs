module Day09 (run) where

import Data.Bifunctor(bimap)
import Data.Set as Set (fromList, toList)

-- syntactic sugar
type Position = (Int, Int)
type Step = Int
data Direction = U | D | L | R | UL | UR | DL | DR deriving (Show, Eq, Enum, Read)

-- move the index in a direction by 1
moveIndex :: Direction -> Position -> Position
moveIndex direction (x,y) = case direction of
  U -> (x, y-1);  UL -> (x-1, y-1)
  D -> (x, y+1);  UR -> (x+1, y-1)
  L -> (x-1, y);  DL -> (x-1, y+1)
  R -> (x+1, y);  DR -> (x+1, y+1)

-- find the direction according to the position of the previous node to the next node
directionFinder :: Position -> Position -> Direction
directionFinder (xh, yh) (xt, yt) 
  | xt < xh && yt < yh  = DR
  | xt > xh && yt < yh = DL
  | xt < xh && yt > yh =  UR
  | xt > xh && yt > yh = UL
  | yt < yh = D
  | yt > yh = U
  | xt < xh = R
  | xt > xh = L
  | otherwise = error "directionFinder: undefined"

-- is a node around another node?
nodeAroundNode :: Position -> Position -> Bool 
nodeAroundNode (xh, yh) (xt, yt) = or [(xt, yt) == (xh+i, yh+j) | i <- [-1, 0, 1], j <- [-1, 0, 1]]

-- to let the previous node follow the next node
followNext :: Position -> Position -> Position
followNext nextPos prevPos
  | nodeAroundNode nextPos prevPos = prevPos
  | otherwise = moveIndex (directionFinder nextPos prevPos) prevPos

-- move the rope and all its tails 
moveNodes :: Direction -> Step ->  [Position] -> [[Position]]
moveNodes _ 0 ps = [ps]
moveNodes direction len ps = ps : moveNodes direction (len-1) ps''
  where ps'' = scanl1 followNext ps' -- scan is fold that retains the process
        ps' = moveIndex direction (head ps) : tail ps

-- move the rope with all the steps in the directions
moveRope :: [(Direction, Step)] -> [Position] -> [[Position]]
moveRope [] ps = [ps]
moveRope ((direction, len) : xs) ps = ys ++ moveRope xs ps'
  where ps' = if null ys then ps else last ys
        ys = moveNodes direction len ps

-- distinct positions of the tail
traceTail :: [[Position]] -> [Position]
traceTail = Set.toList . Set.fromList . map last

-- Init rope can be [Head, Tail] or [Head .. Tail]
initRope :: Int -> [Position]
initRope = flip replicate (0,0)

-- Extra: pretty print the grid for tail positions 😉
prettyPrint :: [Position] -> IO ()
prettyPrint ps = mapM_ putStrLn fillGrid
  where fillGrid = [[if (x,y) `elem` ps then 'X' else '.' | x <- [minX..maxX]] | y <- [minY..maxY]]
        (minX, minY) = (minimum . map fst $ ps, minimum . map snd $ ps) 
        (maxX, maxY) = (maximum . map fst $ ps, maximum . map snd $ ps)

-- main, assuming input is valid.
run :: IO ()
run = do 
  input <- map (bimap (\x->read x::Direction) (\x->read x::Step) . splitAt 1) . lines <$> readFile "input.data"
  putStr "Visited Positions of the rope's tail: "
  let part1 = traceTail . moveRope input $ initRope 2
  print (length part1) >> prettyPrint part1
  putStr "Visited Positions of the its extended tail: "
  let part2 = traceTail . moveRope input $ initRope 10
  print (length part2) >> prettyPrint part2
