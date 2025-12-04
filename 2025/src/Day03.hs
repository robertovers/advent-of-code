module Day03 (solve) where

import Data.List (elemIndex)
import Lib

solve :: IO ()
solve = do
  input <- readInput 3
  putStrLn $ "Part 1: " ++ show (joltage 2 (map parseLine input))
  putStrLn $ "Part 2: " ++ show (joltage 12 (map parseLine input))

parseLine :: String -> [Int]
parseLine = map (\x -> read [x])

-- part 1 and 2

joltage :: Int -> [[Int]] -> Int
joltage n = foldr ((+) . joinInts . largest n) 0

largest :: Int -> [Int] -> [Int]
largest 1 xs = [maximum xs]
-- greedily pick the biggest battery leaving at least (n-1) remaining, then recurse
largest n xs = case elemIndex greedy xs of
  Just i -> greedy : largest (n - 1) (drop (i + 1) xs)
  Nothing -> error ""
  where
    greedy = maximum (take (length xs - (n - 1)) xs)

-- helpers

joinInts :: [Int] -> Int
joinInts = read . concatMap show
