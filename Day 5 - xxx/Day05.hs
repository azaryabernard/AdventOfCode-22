module Day05 (run) where

-- main, assuming input is valid. Yes, I love one liners. :)
run :: IO ()
run = do 
  _ <- lines <$> readFile "input.data"
  putStrLn "todo"
