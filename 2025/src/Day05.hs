module Day05 (solve) where

import Lib

solve :: IO ()
solve = do
    input <- readInput 5
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

part1 :: [String] -> Int
part1 _ = 0

part2 :: [String] -> Int
part2 _ = 0
