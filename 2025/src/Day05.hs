module Day05 (solve) where

import Data.List
import Lib
import Text.Megaparsec
import Text.Megaparsec.Char

solve :: IO ()
solve = do
  input <- readInput 5
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

-- part 1

part1 :: [String] -> Int
part1 input = length $ filter id (map (isFresh ranges) ids)
  where
    splt = splitInput input
    ranges = parseRanges $ fst splt
    ids = parseIds $ drop 1 (snd splt)

isFresh ranges id = foldr (\x -> (||) (id > fst x && id < snd x)) False ranges

-- part 2

part2 :: [String] -> Int
part2 input = foldr (\x -> (+) (snd x - fst x + 1)) 0 ranges
  where
    ranges = mergeRanges $ sort $ parseRanges $ fst (splitInput input)

-- sorted, so assume ax <= bx
mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges [] = []
mergeRanges [(ax, ay)] = [(ax, ay)]
mergeRanges [(ax, ay), (bx, by)]
  | bx <= ay && by >= ay = [(ax, by)]
  | bx <= ay && by <= ay = [(ax, ay)]
  | otherwise = [(ax, ay), (bx, by)]
mergeRanges (axs : bxs : rest) = case mergeRanges [axs, bxs] of
  [_, _] -> axs : mergeRanges (bxs : rest)
  [m] -> mergeRanges (m : rest)

-- helpers

splitInput :: [String] -> ([String], [String])
splitInput input = case idx of
  Just i -> splitAt i input
  Nothing -> error "bad input"
  where
    idx = elemIndex "" input

parseIds :: [String] -> [Int]
parseIds input = case traverse (parse parseInt "") input of
  Left _ -> error "bad input"
  Right ids -> ids

parseInt :: Parser Int
parseInt = do read <$> some digitChar

parseRanges :: [String] -> [(Int, Int)]
parseRanges input = case traverse (parse parseRange "") input of
  Left _ -> error "bad input"
  Right ranges -> ranges

parseRange :: Parser (Int, Int)
parseRange = do
  firstId <- read <$> some digitChar
  _ <- char '-'
  lastId <- read <$> some digitChar
  return (firstId, lastId)
