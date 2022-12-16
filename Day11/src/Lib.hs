module Lib
    ( someFunc
    ) where

import Data.Function (on)
import Data.List (groupBy, sortOn, sort)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import qualified Data.Char as Char

type Item = Int
type Operation = (Item -> Item)
type Test = (Item -> Bool)
type Inspection = (Monkey -> Item -> Item)

data Monkey = Monkey
  { monkeyIndex   :: Int
  , startingItems :: [Item]
  , operation     :: Operation
  , test          :: Test
  , ifTrue        :: Int
  , ifFalse       :: Int
  }

instance Show Monkey where
    show m = "Monkey" ++ show (monkeyIndex m)

someFunc :: IO ()
someFunc = do
    args <- getArgs
    input <- getInput $ head args
    let round0 = Round input (map startingItems input) []
    print $ run 20 round0 inspect
    print $ run 1000 round0 inspect'


getInput :: FilePath -> IO [Monkey]
getInput = fmap (map parseMonkey . splitOn [""] . lines) . readFile

parseMonkey :: [String] -> Monkey
parseMonkey [mi, si, o, t, itr, ifa] = Monkey 
    { 
        monkeyIndex   = (parseMonkeyIndex mi),
        startingItems = (parseStartingItems si), 
        operation     = (parseOperation o),
        test          = (parseTest t),
        ifTrue        = (parseTrue itr),
        ifFalse       = (parseFalse ifa)
    }

parseMonkeyIndex :: String -> Int
parseMonkeyIndex = read . takeWhile Char.isDigit . drop 7

parseStartingItems :: String -> [Item]
parseStartingItems = map read . splitOn ", " . drop 18

parseOperation :: String -> Operation
parseOperation = parseOperation' . drop 23
    where parseOperation' "* old"  = (\i -> i * i)
          parseOperation' ('*':xs) = (\i -> i * (read xs))
          parseOperation' ('+':xs) = (\i -> i + (read xs))

parseTest :: String -> Test
parseTest = parseTest' . drop 21
    where parseTest' xs = (\i -> i `mod` (read xs) == 0)

parseTrue :: String -> Int
parseTrue = read . drop 29

parseFalse :: String -> Int
parseFalse = read . drop 30

data Round = Round
  { monkeys :: [Monkey]
  , queue   :: [[Item]]
  , history :: [(Monkey, Item)]
  } deriving (Show)

inspect :: Inspection
inspect m i = (operation m i) `div` 3

inspect' :: Inspection
inspect' m i = (operation m i)

throw :: Inspection -> Monkey -> Round -> Item -> Round
throw inspection m r i 
    | i' <- inspection m i
    , q <- queue r
    , tm <- test m i'
    , (hs, (t:ts)) <- splitAt (testResult tm) q 
    , hist <- history r
    = r { queue = hs ++ ((t ++ [i']):ts), history = (m, i):hist }
    where testResult True  = ifTrue m
          testResult False = ifFalse m

doRound :: Inspection -> Round -> Round
doRound inspection r@(Round ms _ _) = foldl (doRound' inspection) r ms

doRound' :: Inspection -> Round -> Monkey -> Round
doRound' inspection r m 
    | mi <- monkeyIndex m
    , (Round _ q' h') <- foldl (throw inspection m) r ((queue r)!!mi)
    , (qs, (_:qt)) <- splitAt mi q'
    = r { queue = qs ++ ([]:qt), history = h' }

run :: Int -> Round -> Inspection -> Int
run it r0 is = product . take 2 . reverse . sort $ run' it r0 is 

run' :: Int -> Round -> Inspection -> [Int]
run' it r0 is = map length $ groupBy ((==) `on` (monkeyIndex . fst)) $ sortOn (monkeyIndex . fst) $ history $ (iterate (doRound is) r0) !! it