module Day05 (day05) where

-- main, assuming input is valid. Yes, I love one liners. :)
day05 :: IO ()
day05 = do 
  input <- lines <$> readFile "input.data"
  print input
