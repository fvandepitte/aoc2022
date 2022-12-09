module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import Data.List (nub)

type Movement = (Char, Int)
type Coord    = (Int, Int)

someFunc :: IO ()
someFunc = do
    args <- getArgs
    input <- getInput $ head args
    putStrLn ("Debug")
--    putStrLn (show . map last . scanl move (replicate 2 (0,0)) . concat . map calculateMoves $ input)
    putStrLn ("Part 1: " ++ show (part1 input))
    putStrLn ("Part 2: " ++ show (part2 input))

getInput :: FilePath -> IO [Movement]
getInput = fmap (map parseLine . lines) . readFile

parseLine :: String -> Movement
parseLine (c:_:xs) = (c, read xs)
parseLine _         = undefined

add :: Coord -> Coord -> Coord
add (a, b) (c, d) = (a + c, b + d)

move :: [Coord] -> (Int, Int) -> [Coord]
move (h:ts) m
    | h' <- add h m = (h'):(move'' h' ts)

move'' :: Coord -> [Coord] -> [Coord]
move'' _ [] = []
move'' p (k:ks) 
    | k' <- moveTail k p = k':(move'' k' ks)

calculateMoves :: Movement -> [(Int, Int)]
calculateMoves (c, n) = replicate n $ calculateMoves' c
    where calculateMoves' 'R' = ( 1, 0)
          calculateMoves' 'L' = (-1, 0)
          calculateMoves' 'U' = ( 0, 1)
          calculateMoves' 'D' = ( 0,-1)
          calculateMoves' _   = ( 0, 0)

moveTail :: Coord -> Coord -> Coord
moveTail t@(tX, tY) (hX, hY) 
        | touches   = t
        | otherwise = (tX + signum (hX - tX), tY + signum (hY - tY)) 
    where touches = (abs (tX - hX)) <= 1 && (abs (tY - hY)) <= 1 

part1 :: [Movement] -> Int
part1 = calculateTailMovement 2

part2 :: [Movement] -> Int
part2 = calculateTailMovement 10

calculateTailMovement :: Int -> [Movement] -> Int
calculateTailMovement l ms = length . nub . map last . scanl move (replicate l (0,0)) . concat . map calculateMoves $ ms