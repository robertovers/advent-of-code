module Day06 (solve) where

import Lib
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List
import Data.Either

solve :: IO ()
solve = do
    input <- readInput 6
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

-- part 1

part1 :: [String] -> Int
part1 input = case traverse (parse (some parseToken) "") input of
  Left _ -> error "bad input"
  Right mtrx -> sum $ map (calcColumn nums ops) [0 .. length (head nums) - 1]
    where
      nums = map lefts $ take (length mtrx - 1) mtrx
      ops = rights $ mtrx !! (length mtrx - 1)

calcColumn :: [[Int]] -> [Char] -> Int -> Int
calcColumn nums ops i = foldr (op . (!! i)) initl nums
  where
    op = if ops !! i == '+' then (+) else (*)
    initl = if ops !! i == '+' then 0 else 1

parseToken :: Parser (Either Int Char)
parseToken = do
  _ <- many (char ' ')
  x <- choice [Left . read <$> some digitChar, Right <$> oneOf "+*"]
  _ <- many (char ' ')
  return x

-- part 2

part2 :: [String] -> Int
part2 input = case traverse (parse parseToken2 "") (transpose input) of
  Left _ -> error "bad input"
  Right x -> sum $ part2' (+) [0] x

part2' :: (Int -> Int -> Int) -> [Int] -> [(Maybe Int, Maybe Char)] -> [Int]
part2' _ res [] = res
part2' op (r:rs) ((Just x, Nothing) : xs) = part2' op (op x r : rs) xs
part2' op (r:rs) ((Nothing, Nothing) : xs) = part2' op (r : rs) xs
part2' _ (r:rs) ((Just x, Just o) : xs) = part2' (if o == '+' then (+) else (*)) (x : r : rs) xs
part2' _ (r:rs) ((Nothing, Just o) : xs) = part2' (if o == '+' then (+) else (*)) (r : rs) xs

parseToken2 :: Parser (Maybe Int, Maybe Char)
parseToken2 = do
  _ <- many (char ' ')
  x <- optional $ read <$> some digitChar
  _ <- many (char ' ')
  op <- optional $ oneOf "+*"
  _ <- many (char ' ')
  return (x, op)
