module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import qualified Data.Char as Char
import Data.List.Split (chunksOf)

data Instruction = Noop | Addx Int deriving (Show, Read)

someFunc :: IO ()
someFunc = do
    args <- getArgs
    input <- getInput $ head args
    let fs = calculateFrequencies input
    putStrLn ("Part 1: " ++ show (part1 fs))
    putStrLn ("Part 2:\n" ++ part2 fs)

getInput :: FilePath -> IO [Instruction]
getInput = fmap (map (read . capitalized) . lines) . readFile

capitalized :: String -> String
capitalized (x:xs) = Char.toUpper x : map Char.toLower xs
capitalized [] = []

executeInstruction :: Instruction -> [(Int -> Int)]
executeInstruction Noop     = [id]
executeInstruction (Addx n) = [id, (+n)]

part1 :: [Int] -> Int
part1 fs = sum $ map (\x -> (*x) $ fs !! (x - 1)) [20, 60, 100, 140, 180, 220]

part2 :: [Int] -> String
part2 = unlines . chunksOf 40 . init . zipWith (\a b -> if (abs (a - b) <= 1) then '#' else '.') (cycle [0..39])

calculateFrequencies :: [Instruction] -> [Int]
calculateFrequencies = scanl (\x fn -> fn x) 1 . concat . map executeInstruction 