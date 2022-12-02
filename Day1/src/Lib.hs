module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import Data.List (sort)
import Data.List.Split (splitOn)

someFunc :: IO ()
someFunc = do
    args <- getArgs
    p1Solution <- solve part1 $ head args
    p2Solution <- solve part2 $ head args
    putStrLn ("Part 1: " ++ show p1Solution)
    putStrLn ("Part 2: " ++ show p2Solution)

solve :: ([[Integer]] -> b) -> FilePath -> IO b
solve fn = fmap fn . getInput

getInput :: FilePath -> IO [[Integer]]
getInput = fmap (makeIntegers . splitOn [""] . lines) . readFile
    where makeIntegers = map (map read)

part1 :: [[Integer]] -> Integer
part1 = maximum . map sum

part2 :: [[Integer]] -> Integer
part2 = sum . take 3 . reverse . sort . map sum