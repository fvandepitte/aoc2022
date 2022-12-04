module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Tuple (swap)

someFunc :: IO ()
someFunc = do
    args <- getArgs
    p1Solution <- solve part1 $ head args
    putStrLn ("Part 1: " ++ show p1Solution)
    p2Solution <- solve part2 $ head args
    putStrLn ("Part 2: " ++ show p2Solution)

solve :: (((Int, Int), (Int, Int)) -> Bool) -> FilePath -> IO Int
solve fn = fmap (length . filter id . map fn) . getInput

getInput :: FilePath -> IO [((Int, Int), (Int, Int))]
getInput = fmap (map parseLine . lines) . readFile

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine = toTuple . map (toTuple . map read . splitOn "-") . splitOn ","
    where toTuple [a, b] = (a,b)

part1 :: ((Int, Int), (Int, Int)) -> Bool
part1 x = overlaps x || overlaps (swap x)
    where overlaps ((a,b),(c,d)) =  a <= c && b >= d

part2 :: ((Int, Int), (Int, Int)) -> Bool
part2 x = overlaps x || overlaps (swap x)
    where overlaps ((a,b),(c,d)) = a <= c && c <= b || a <= d && d <= b 