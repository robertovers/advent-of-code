module Day07 (solve) where

import Lib
import Data.List

solve :: IO ()
solve = do
    input <- readInput 7
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

-- part 1

part1 :: [String] -> Int
part1 input = snd $ foldl (flip update) (strt, 0) rest 
  where
    strt = fst $ parseInput input
    rest = snd $ parseInput input

update :: [Int] -> ([Int], Int) -> ([Int], Int)
update splits (beams, acc) = (nextb, acc + acc')
  where
    acc' = sum $ map (snd . splitBeam splits) beams
    nextb = nub $ concatMap (fst . splitBeam splits) beams

splitBeam :: [Int] -> Int -> ([Int], Int)
splitBeam splits beam = if beam `elem` splits
  then ([beam-1, beam+1], 1)
  else ([beam], 0)

-- part 2

-- keep a count of the no. of beams at each x coordinate, then sum at the end
part2 :: [String] -> Int
part2 input = sum $ map snd $ foldl (flip update2) [(strt, 1)] rest 
  where
    strt = head $ fst $ parseInput input
    rest = snd $ parseInput input

update2 :: [Int] -> [(Int, Int)] -> [(Int, Int)]
update2 splits beams = concatCounts $ sort $ concatMap (splitBeam2 splits) beams

splitBeam2 :: [Int] -> (Int, Int) -> [(Int, Int)]
splitBeam2 splits (beam, n) = if beam `elem` splits
  then [(beam-1,n), (beam+1,n)]
  else [(beam, n)]

concatCounts :: [(Int, Int)] -> [(Int, Int)]
concatCounts ((x, cx):(y, cy):rest)
  | x == y = (x, cx + cy) : concatCounts rest
  | otherwise = (x, cx) : concatCounts ((y, cy) : rest)
concatCounts x = x 

-- helpers

parseInput :: [String] -> ([Int], [[Int]])
parseInput input = (strt, rest)
  where
    strt = 'S' `elemIndices` head input
    rest = filter (/= []) $ map (elemIndices '^') (drop 1 input)
