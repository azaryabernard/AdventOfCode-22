module Day04 (day04) where

-- main, assuming input is valid
day04 :: IO ()
day04 = do
  input <- lines <$> readFile "input.data"
  print input
  return ()