module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import Data.Char (isSpace)
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn)
import Text.Regex.Posix

someFunc :: IO ()
someFunc = do
    args <- getArgs
    p1Solution <- solve part1 $ head args
    putStrLn ("Part 1: " ++ show p1Solution) 
    p2Solution <- solve part2 $ head args
    putStrLn ("Part 2: " ++ show p2Solution)

solve :: (([String], [(Int, Int, Int)]) -> b) -> FilePath -> IO b
solve fn = fmap fn . getInput

getInput :: FilePath -> IO ([String], [(Int, Int, Int)])
getInput = fmap (parseInputLines . lines) . readFile

parseInputLines :: [String] -> ([String], [(Int, Int, Int)])
parseInputLines = parseInputLines' . splitOn [""]
    where parseInputLines' [a, b] = (parseInitialList a, map parseInstructionLine b)
    
parseInitialList :: [String] -> [String]
parseInitialList = map concat . map (filter (/=[]) . init) . transpose . map (map trim . chunksOf 4)

trim :: String -> String
trim = reverse . dropWhile  trim' . reverse . dropWhile  trim'
    where trim' c = isSpace c || c == '[' || c == ']'

parseInstructionLine :: String -> (Int, Int, Int)
parseInstructionLine = parseInstructionLine''' . parseInstructionLine'' . parseInstructionLine'
    where parseInstructionLine' :: String -> (String,String,String,[String])
          parseInstructionLine' xs = xs =~ "move ([1-9][0-9]*) from ([1-9]+) to ([1-9]+)"
          parseInstructionLine'' :: (String,String,String,[String]) -> [Int]
          parseInstructionLine'' (_, _, _, xss) = map read xss 
          parseInstructionLine''' :: [Int] -> (Int, Int, Int)
          parseInstructionLine''' [a, b, c] = (a, b, c)
          parseInstructionLine''' _         = (0, 0, 0)

popInList :: [[a]] -> Int -> Maybe (a, [[a]])
popInList xss i
    | (y, x:xs) <- splitAt (i - 1) xss, Just (p, ps) <- pop x = Just (p, y ++ (ps:xs))
    | otherwise                                               = Nothing

pushInList :: [[a]] -> Int -> a -> [[a]]
pushInList xss i a
    | (y, x:xs) <- splitAt (i - 1) xss = y ++ ((a:x):xs)

moveInList :: Int -> Int -> [[a]] ->  [[a]]
moveInList f t xss
    | Just (x, xss') <- popInList xss f = pushInList xss' t x
    | otherwise                         = xss

executeInstructionLine :: [[a]] -> (Int, Int, Int) -> [[a]]
executeInstructionLine xss (n, f, t) = (iterate (moveInList f t) xss) !! n

executeBulkInstructionLine :: [[a]] -> (Int, Int, Int) -> [[a]]
executeBulkInstructionLine xss (n, f, t)
    | (y, x:xs) <- splitAt (f - 1) xss, -- pick the stack to move
      (z, zs) <- splitAt n x, -- pick the items to move
      xss' <- y ++ (zs:xs), -- recreate stack without to move items
      (a, b:c) <- splitAt (t - 1) xss' -- pick the stack to put
        = a ++ ((z ++ b):c) -- bring it all toghether

pop :: [a] -> Maybe (a, [a])
pop [a]    = Just (a, [])
pop (a:as) = Just (a, as)
pop _      = Nothing

part1 :: ([String], [(Int, Int, Int)]) -> String
part1 (xs, cls) = map head $ foldl (executeInstructionLine) xs cls

part2 :: ([String], [(Int, Int, Int)]) -> String
part2 (xs, cls) = map head $ foldl (executeBulkInstructionLine) xs cls