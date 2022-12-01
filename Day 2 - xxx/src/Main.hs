module Main (main) where

main :: IO ()
main = do
  putStrLn "hello world"
  readFile "src/data" >>= putStrLn
