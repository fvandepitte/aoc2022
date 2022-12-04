module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import Data.List (intersect)
import Data.List.Split (chunksOf)
import Data.Char (ord, isUpper)

someFunc :: IO ()
someFunc = do
    args <- getArgs
    p1Solution <- solve part1 $ head args
    putStrLn ("Part 1: " ++ show p1Solution)
    p2Solution <- solve part2 $ head args
    putStrLn ("Part 2: " ++ show p2Solution)

solve :: ([String] -> b) -> FilePath -> IO b
solve fn = fmap fn . getInput

getInput :: FilePath -> IO [String]
getInput = fmap lines . readFile

part1 :: [String] -> Int
part1 = sum . map (scoreChar . findDuplicateItem)

part2 :: [String] -> Int
part2 = sum . map (scoreChar . findBadge) . chunksOf 3

findBadge :: [String] -> Char
findBadge (x:xs) = head $ foldr (intersect) x xs

findDuplicateItem :: String -> Char
findDuplicateItem xs = head $ uncurry intersect $ splitAt middle xs
    where middle = fst $ divMod (length xs) 2

scoreChar :: Char -> Int
scoreChar c 
    | isUpper c = ord c - 38
    | otherwise = ord c - 96
