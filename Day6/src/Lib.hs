module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import Data.List (elemIndex, tails, nub)
import Data.Maybe (fromJust)

someFunc :: IO ()
someFunc = do
    args <- getArgs
    p1Solution <- solve part1 $ head args
    putStrLn ("Part 1: " ++ show p1Solution)
    p2Solution <- solve part2 $ head args
    putStrLn ("Part 2: " ++ show p2Solution)

solve :: (String -> b) -> FilePath -> IO b
solve fn = fmap fn . getInput

getInput :: FilePath -> IO String
getInput = readFile

part1 :: String -> Int
part1 = (+) 4 . fromJust . elemIndex True . map ((\xs -> (length xs) == (length $ nub xs)) . take 4) . tails

part2 :: String -> Int
part2 = (+) 14 . fromJust . elemIndex True . map ((\xs -> (length xs) == (length $ nub xs)) . take 14) . tails