module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import Data.List.Split (splitOn)

someFunc :: IO ()
someFunc = do
    args <- getArgs
    p1Solution <- solve part1 $ head args
    putStrLn ("Part 1: " ++ show p1Solution)
    p2Solution <- solve part2 $ head args
    putStrLn ("Part 2: " ++ show p2Solution)

solve :: ([((Int, Int), (Int, Int))] -> b) -> FilePath -> IO b
solve fn = fmap fn . getInput

getInput :: FilePath -> IO [((Int, Int), (Int, Int))]
getInput = fmap (map parseLine . lines) . readFile

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine = toTuple . map (toTuple . map read . splitOn "-") . splitOn ","
    where toTuple [a, b] = (a,b)

part1 :: [((Int, Int), (Int, Int))] -> Int
part1 = sum . map countOverlap

part2 :: [((Int, Int), (Int, Int))] -> Int
part2 = sum . map countOverlap2

countOverlap :: ((Int, Int), (Int, Int)) -> Int
countOverlap ((a,b),(c,d))
    | a <= c && b >= d = 1
    | c <= a && d >= b = 1
    | otherwise        = 0

countOverlap2 :: ((Int, Int), (Int, Int)) -> Int
countOverlap2 ((a,b),(c,d))
    | a <= c && c <= b = 1
    | a <= d && d <= b = 1
    | c <= a && a <= d = 1
    | c <= b && b <= d = 1
    | otherwise        = 0