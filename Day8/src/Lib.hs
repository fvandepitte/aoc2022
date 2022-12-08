module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import Data.Matrix
import Data.Char (digitToInt, isDigit)
import qualified Data.Vector as V

type Forrest = Matrix Int
type Coord = (Int, Int)

someFunc :: IO ()
someFunc = do
    args <- getArgs
    input <- getInput $ head args
    putStrLn ("Input:\n" ++ show input)
    putStrLn ("Rows: " ++ show (nrows input))
    putStrLn ("Cols: " ++ show (ncols input))
    p1Solution <- solve part1 $ head args
    putStrLn ("Part 1: " ++ show p1Solution)
    p2Solution <- solve part2 $ head args
    putStrLn ("Part 2 map:\n" ++ show (mapPos (calculateScenicScore input) input))
    putStrLn ("Part 2: " ++ show p2Solution)

solve :: (Forrest -> b) -> FilePath -> IO b
solve fn = fmap fn . getInput

getInput :: FilePath -> IO Forrest
getInput = fmap parseInput . readFile

-- "30373\r\n25512\r\n65332\r\n33549\r\n35390"
parseInput :: String -> Forrest
parseInput = fromLists . map (map digitToInt . filter isDigit) . lines

isTreeVisible :: Forrest -> Coord -> Int -> Bool
isTreeVisible _ (1, _) _               = True
isTreeVisible _ (_, 1) _               = True
isTreeVisible f c@(y, x) h 
    | x == (ncols f) || y == (nrows f) = True -- All the edge cases ^^, you get it? `Edge` case
    | otherwise                        = isTreeVisible'' f c h

isTreeVisible'' :: Forrest -> Coord -> Int -> Bool
isTreeVisible'' f (y, x) h 
    | (l, r) <- V.splitAt (x - 1) $ getRow y f,
      (a, b) <- V.splitAt (y - 1) $ getCol x f 
        = any id $ map (V.all ((>)h)) [l, V.tail r, a, V.tail b]

calculateScenicScore  :: Forrest -> Coord -> Int -> Int
calculateScenicScore f (y, x) h 
    | y == 1 || y == (nrows f) || x == 1 || x == (ncols f) = 0
    | (l, r) <- V.splitAt (x - 1) $ getRow y f,
      (a, b) <- V.splitAt (y - 1) $ getCol x f 
        = product $ map (length . takeWhileInclusive id . map ((>)h) . V.toList) [V.reverse l, V.tail r, V.reverse a, V.tail b]

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

part1 :: Forrest -> Int
part1 f = length $ filter id $ toList $ mapPos (isTreeVisible f) f

part2 :: Forrest -> Int
part2 f = maximum $ toList $ mapPos (calculateScenicScore f) f