module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Char (isPrint)

type Path = [String]
type LookupSystem = [(Path, [FileSystem])]

-- Example = let root = Dir "/" [Dir "a" [Dir "e" [File "i" 584], File "f" 29116, File "g" 2557, File "h.lst" 62596], File "b.txt" 14848514, File "c.dat" 8504156, Dir "d" [File "j" 4060174, File "d.log" 8033020, File "d.ext" 5626152, File "k" 7214296]]
data FileSystem = Dir Path [FileSystem] | File String Int
instance Eq FileSystem where
    (Dir _ _)  == (File _ _) = False
    (File _ _) == (Dir _ _)  = False
    (Dir x _)  == (Dir y _)  = x == y
    (File x _) == (File y _) = x == y

instance Show FileSystem where
    show fs = showFileSystem 0 fs

diskSpace :: Int 
diskSpace = 70000000

spaceReq :: Int
spaceReq = 30000000

showFileSystem :: Int -> FileSystem -> String
showFileSystem l d@(Dir n xs) = init $ unlines $ [(replicate (l * 2) ' ') ++ "- " ++ last n ++ " (dir, size="++ show (getSize d) ++")"] ++ map (showFileSystem (l+1)) xs
showFileSystem l (File n s) = (replicate (l * 2) ' ') ++ "- " ++ n ++ " (file, size=" ++ show s ++ ")"

createCommands :: String -> [[String]]
createCommands = map (map (filter isPrint) . lines) . tail . splitOn "$ "

parseCommand ::  (Path, LookupSystem) -> [String] -> (Path, LookupSystem)
parseCommand (p, l) [a] = (changeDir p (drop 3 a), l)
parseCommand (p, l) xs = (p, (p, map (parseLsLine p) $ tail xs):l)

parseLsLine :: Path -> String -> FileSystem
parseLsLine p xs 
    | "dir" `isPrefixOf` xs              = Dir (p ++ [(drop 4 xs)]) []
    | [s,n] <- splitOn " " xs, otherwise = File n (read s)

replaceDirectories :: LookupSystem -> FileSystem -> FileSystem
replaceDirectories _ f@(File _ _) = f
replaceDirectories ls (Dir n _) | Just fs <- lookup n ls = Dir n (map (replaceDirectories ls) fs) 

someFunc :: IO ()
someFunc = do
    args <- getArgs
    input <- getInput $ head args
    putStrLn ("Input: " ++ show input)
    p1Solution <- solve part1 $ head args
    putStrLn ("Part 1: " ++ show p1Solution)
    p2Solution <- solve part2 $ head args
    putStrLn ("Part 2: " ++ show p2Solution)

solve :: (FileSystem -> b) -> FilePath -> IO b
solve fn = fmap fn . getInput

getInput :: FilePath -> IO FileSystem
getInput = fmap (parseInput) . readFile

parseInput :: String -> FileSystem
parseInput = flip replaceDirectories (Dir ["/"] []) . snd . foldl parseCommand ([],[]) . createCommands

getSize :: FileSystem -> Int
getSize (File _ s) = s
getSize (Dir _ xs) = sum $ map getSize xs

listDirectories :: FileSystem -> [(Path, Int)]
listDirectories (File _ _) = []
listDirectories d@(Dir n xs) = (n, getSize d):(concat $ map listDirectories xs)

changeDir :: Path -> String -> Path
changeDir _ "/"   = ["/"]
changeDir xs ".." = init xs
changeDir xs a    = xs ++ [a]

part1 :: FileSystem -> Int
part1 = sum . filter (<=100000) . map snd . listDirectories

part2 :: FileSystem -> Int
part2 fs = (minimum . filter (>=treshHold) . map snd . listDirectories) fs
    where treshHold = spaceReq - (diskSpace - (getSize fs))