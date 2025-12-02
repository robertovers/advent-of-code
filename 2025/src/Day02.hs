module Day02 (solve) where

import Lib
import Data.List.Split
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace

debug = flip trace

solve :: IO ()
solve = do
    input <- splitOn "," <$> readInputFile 2
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

parseIdRange :: Parser (Int, Int)
parseIdRange = do
  firstId <- read <$> some digitChar
  _ <- char '-'
  lastId <- read <$> some digitChar
  return (firstId, lastId)

countInvalids :: Int -> (Int, Int) -> Int
countInvalids 1 (s, e) = foldl (\acc x -> acc + isInvalid (show x)) 0 [s..e]
countInvalids 2 (s, e) = foldl (\acc x -> acc + isInvalid2 (show x)) 0 [s..e]

-- Part 1

isInvalid :: String -> Int
isInvalid str = case mod (length str) 2 of
  0 -> if fst split' == snd split' then (read str) else 0
  _ -> 0
  where
    split' = splitAt (div (length str) 2) str

part1 :: [String] -> Maybe Int
part1 input = case traverse (parse parseIdRange "") input of
  Left _ -> Nothing
  Right xs -> Just $ foldl (\acc x -> acc + countInvalids 1 x) 0 xs

-- Part 2

isMultiple :: String -> Int -> Bool
isMultiple str n =
  str == concat (replicate (div (length str) n) (take n str))

isInvalid2 :: String -> Int
isInvalid2 str = if
  foldl (\acc n -> acc || (isMultiple str n)) False [1..(div (length str) 2)]
  then read str else 0

part2 :: [String] -> Maybe Int
part2 input = case traverse (parse parseIdRange "") input of
  Left _ -> Nothing
  Right xs -> Just $ foldl (\acc x -> acc + countInvalids 2 x) 0 xs
