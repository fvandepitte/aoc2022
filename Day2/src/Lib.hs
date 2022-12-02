module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)

data HandType = Rock | Paper | Scissors deriving (Eq)

someFunc :: IO ()
someFunc = do
    args <- getArgs
    p1Solution <- solve part1 $ head args
    putStrLn ("Part 1: " ++ show p1Solution)
    p2Solution <- solve part2 $ head args
    putStrLn ("Part 2: " ++ show p2Solution)

solve :: ([(Char, Char)] -> b) -> FilePath -> IO b
solve fn = fmap fn . getInput

getInput :: FilePath -> IO [(Char, Char)]
getInput = fmap (map parseLine . lines) . readFile
    where parseLine [a, _, b] = (a, b)

part1 :: [(Char, Char)] -> Integer
part1 = foldr ((+) . calculateScore . parseMatch) 0
    where parseMatch (a, b) = (parseHand a, parseHand b)

part2 :: [(Char, Char)] -> Integer
part2 = foldr ((+) . calculateScore . parseMatch) 0
    where parseMatch (a, b) | c <- parseHand a, True = (c, calculateOutCome c b)
     
calculateScore :: (HandType, HandType) -> Integer
calculateScore (a, b) = sum [calculateWinner a b, calculateHand b]

parseHand :: Char -> HandType
parseHand c 
    | c == 'A' || c == 'Y' = Rock
    | c == 'B' || c == 'X' = Paper
    | c == 'C' || c == 'Z' = Scissors

-- A Rock, B Paper, C Scissors
-- X Rock, Y Paper, Z Scissors
-- Lost 0, Draw 3, Win 6
calculateWinner :: HandType -> HandType -> Integer
calculateWinner Rock Paper      = 6 
calculateWinner Rock Scissors   = 0 
calculateWinner Paper Scissors  = 6
calculateWinner Paper Rock      = 0
calculateWinner Scissors Rock   = 6
calculateWinner Scissors Paper  = 0 
calculateWinner _ _ = 3

calculateOutCome :: HandType -> Char -> HandType
calculateOutCome h 'Y'        = h
calculateOutCome Rock 'X'     = Scissors
calculateOutCome Rock 'Z'     = Paper
calculateOutCome Paper 'X'    = Rock
calculateOutCome Paper 'Z'    = Scissors
calculateOutCome Scissors 'X' = Paper
calculateOutCome Scissors 'Z' = Rock

calculateHand :: HandType -> Integer
calculateHand Rock      = 1
calculateHand Paper     = 2
calculateHand Scissors  = 3