module Day07 (run) where
import Data.List.Split (splitOn)

-- create a file system structure with sizes
type FileSize = Int
type FileName = String
data FS = File FileName FileSize | Dir FileName [FS]
 deriving (Eq, Ord)

instance Show FS where
  show (File n s) = n ++ " " ++ Prelude.show s
  show dir@(Dir n fs) = n ++ " " ++ Prelude.show (getSize dir) 
    ++ concatMap (('\n':). show) fs

-- get the size of FS
getSize :: FS -> Int
getSize (File _ s) = s
getSize (Dir _ fs) = sum $ map getSize fs

-- traverse the file system and put all sizes of DIR in a list
traverseSizes :: FS -> [Int]
traverseSizes (File _ _) = []
traverseSizes dir@(Dir _ fs) = getSize dir : concatMap traverseSizes fs

-- get absolute or relative path of a file or directory
getPath :: Bool -> FS -> FileName
getPath absolute (File n _) = if absolute then n else last . splitOn "/" $ n
getPath absolute (Dir n _) = if absolute then n else last . init . splitOn "/" $ n

-- find the directory with the given name or parent of the given directory
findDir :: FileName -> FS -> Maybe FS
findDir _ (File _ _) = Nothing
findDir name (Dir _ fs) = case filter (\f -> name == getPath False f) fs of
  [] -> Nothing
  x:_ -> Just x

-- find the parent of the given directory ($ cd ..)
findParent :: FileName -> FS -> Maybe FS
findParent _ (File _ _) = Nothing
findParent name dir@(Dir _ fs) 
  | name `elem` map (getPath True) fs = Just dir 
  | otherwise = findDir' fs
  where findDir' [] = Nothing
        findDir' (x:xs) = case findParent name x of
          Nothing -> findDir' xs
          Just y  -> Just y

-- insert a file/directory into the file system
insertFS :: FS -> FS -> FS
insertFS (File _ _) _ = error "Can't insert file into file"
insertFS dir@(Dir n fs) f = if f `elem` fs then dir else case f of
  File name s -> Dir n (File (n++name) s : fs)
  Dir name fs'  -> Dir n (Dir (n++name++"/") fs' : fs)

-- update a directory in the file system
updateDir :: FS -> FS -> FS
updateDir f@(File _ _) _ = f
updateDir dir (File _ _) = dir
updateDir (Dir n fs) dir@(Dir name _) 
  | name == n = dir
  | otherwise = Dir n (map (`updateDir` dir) fs)

-- parse command line input into FS
parseCommand :: FS -> FS -> [String] -> FS
parseCommand _ fs [] = fs
parseCommand _ (File _ _) _ = error "Available command only for directories"
parseCommand root dir@(Dir n _) (x:xs) = case words x of
          ["$", "cd", ".."] -> case findParent n root of
            Nothing -> error "Parent not found"
            Just y  -> {-trace ("Debug PTR: " ++ show y ++ "\n")-} y
          ["$", "cd", name] -> case findDir name dir of
            Nothing -> error "Directory not found"
            Just y  -> y
          ["$", "ls"] -> foldl insertFS dir $ map (
            \y -> let ys = words y in 
              if head ys == "dir" then Dir (ys!!1) [] else File (ys!!1) (read (head ys))
            ) (takeWhile (\y-> head y /= '$') xs)
          _ -> dir

-- parse the input file into FS
parse :: FS -> FS -> [String] -> FS
parse root _ [] = root
parse root ptr input@(_:xs) = let ptr' = parseCommand root ptr input 
                              in parse (updateDir root ptr') ptr' xs  

-- main, assuming input is valid. Bro went hard on this one
run :: IO ()
run = do 
  input <- lines <$> readFile "input.data"
  let root = Dir "/" []
  let parsedFS = parse root root (tail input)
  putStrLn "File System Tree: "  >> print parsedFS
  let traversedSizes = traverseSizes parsedFS
  let restrictedSizes = filter (<100000) traversedSizes
  putStrLn "\nSum of Restricted Directory's Sizes: " >> print (sum restrictedSizes)
  let usedSize = maximum traversedSizes
  let sizesToDelete = filter (>= 30000000 - (70000000 - usedSize)) traversedSizes
  putStrLn "Minimum Size to be deleted: " >> print (minimum sizesToDelete)
