module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import Data.Char (isDigit)
import Data.List (elemIndex, elemIndices, sortBy)
import Data.List.Split (splitOn)
import Debug.Trace

data Pattern = Array [Pattern] | Data Int deriving (Show, Eq)

dividerOne = parseLine "[[2]]"
dividerTwo = parseLine "[[6]]"

someFunc :: IO ()
someFunc = do
    args <- getArgs
    input <- getInput $ head args
    print input
    print $ (sortBy isRightOrder . concat . map unpair) input
    putStrLn ("Part 1: " ++ show (part1 input))
    putStrLn ("Part 2: " ++ show (part2 input))

getInput :: FilePath -> IO [(Pattern, Pattern)]
getInput = fmap (map createPair . splitOn [""] . lines) . readFile

createPair :: [String] -> (Pattern, Pattern) 
createPair [a,b] = (parseLine a, parseLine b)

parseLine :: String -> Pattern
parseLine = head . parseLine' []

parseLine' :: [Pattern] -> String -> [Pattern]
parseLine' ps ('[':xs) | e <- findClosingBracketIndex (elemIndices ']' xs) (elemIndices '[' xs), (cs, (_:ts)) <- splitAt e xs = ps ++ [(Array (parseLine' [] cs))] ++ (parseLine' ps ts)
parseLine' ps (']':_)  = ps
parseLine' ps []       = ps
parseLine' ps (',':xs) = parseLine' ps xs
parseLine' ps xs       | (n, ts) <- span (isDigit) xs = ps ++ [Data (read n)] ++ (parseLine' ps ts)

findClosingBracketIndex :: [Int] -> [Int] -> Int
findClosingBracketIndex (x:_) []      = x
findClosingBracketIndex [x]   _       = x
findClosingBracketIndex (x:xs) (y:ys) 
    | x < y     = x
    | otherwise = findClosingBracketIndex xs ys

isRightOrder :: Pattern -> Pattern -> Ordering
isRightOrder (Array lps) (Array rps) = isRightOrder' lps rps
isRightOrder l@(Array _) r@(Data _)  = isRightOrder l (Array [r])
isRightOrder l@(Data _) r@(Array _)  = isRightOrder (Array [l]) r
isRightOrder (Data l) (Data r)       = l `compare` r

isRightOrder' :: [Pattern] -> [Pattern] -> Ordering
isRightOrder' (l:lps) (r:rps) 
    | o <- (isRightOrder l r) = if o /= EQ then o else isRightOrder' lps rps
isRightOrder' []      []      = EQ
isRightOrder' []      _       = LT
isRightOrder' _      []       = GT

part1 :: [(Pattern, Pattern)] -> Int
part1 = sum . map (+1) . elemIndices (LT) . map (uncurry isRightOrder)

part2 :: [(Pattern, Pattern)] -> Int
part2 = calculateKey . sortBy isRightOrder . concat . map unpair

unpair (a, b) = [a, b]
          calculateKey xs 
            | Just one <- elemIndex dividerOne xs, Just two <- elemIndex dividerTwo xs = one * two