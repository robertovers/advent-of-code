module Day02 (solve) where

import Data.List.Split
import Lib
import Text.Megaparsec
import Text.Megaparsec.Char

solve :: IO ()
solve = do
  input <- splitOn "," <$> readInputFile 2
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

-- part 1

part1 :: [String] -> Maybe Int
part1 input = case traverse (parse parseIdRange "") input of
  Left _ -> Nothing
  Right xs -> Just $ foldr ((+) . countInvalids isInvalid) 0 xs

isInvalid :: String -> Int
isInvalid str = case mod (length str) 2 of
  0 -> if fst splt == snd splt then read str else 0
  _ -> 0
  where
    splt = splitAt (div (length str) 2) str

-- part 2

part2 :: [String] -> Maybe Int
part2 input = case traverse (parse parseIdRange "") input of
  Left _ -> Nothing
  Right xs -> Just $ foldr ((+) . countInvalids isInvalid2) 0 xs

isInvalid2 :: String -> Int
isInvalid2 str =
  if foldr ((||) . isMultiple str) False [1 .. (div (length str) 2)]
    then read str
    else 0

isMultiple :: String -> Int -> Bool
isMultiple str n =
  str == concat (replicate (div (length str) n) (take n str))

-- helpers

countInvalids :: (String -> Int) -> (Int, Int) -> Int
countInvalids fn (s, e) = foldr ((+) . (fn . show)) 0 [s .. e]

parseIdRange :: Parser (Int, Int)
parseIdRange = do
  firstId <- read <$> some digitChar
  _ <- char '-'
  lastId <- read <$> some digitChar
  return (firstId, lastId)
