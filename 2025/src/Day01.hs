module Day01 (solve) where

import Lib
import Text.Megaparsec
import Text.Megaparsec.Char

solve :: IO ()
solve = do
  input <- readInput 1
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

-- part 1

part1 :: [String] -> Maybe (Int, Int)
part1 = countZeros update

update :: (Int, Int) -> (Char, Int) -> (Int, Int)
update (current, zeros) instr = case mod (rotate current instr) 100 of
  0 -> (0, zeros + 1)
  n -> (n, zeros)

-- part 2

part2 :: [String] -> Maybe (Int, Int)
part2 = countZeros update2

update2 :: (Int, Int) -> (Char, Int) -> (Int, Int)
-- starting at 0, just count full rotations
update2 (0, zeros) (d, n) = (mod (rotate 0 (d, n)) 100, zeros + div n 100)
-- check if sign is different to get first crossing, followed by full rotations
-- also need to check if we land on 0 at end
update2 (current, zeros) instr =
  (mod new 100, zeros + onZero + initialCross + div (abs new) 100)
  where
    new = rotate current instr
    initialCross = if current * new < 0 then 1 else 0
    onZero = if new == 0 then 1 else 0

-- helpers

countZeros :: ((Int, Int) -> (Char, Int) -> (Int, Int)) -> [String] -> Maybe (Int, Int)
countZeros fn input = case traverse (parse parseInstr "") input of
  Left _ -> Nothing
  Right xs -> Just $ foldl fn (50, 0) xs

rotate :: Int -> (Char, Int) -> Int
rotate current ('L', n) = current - n
rotate current ('R', n) = current + n
rotate _ _ = error "bad input"

parseInstr :: Parser (Char, Int)
parseInstr = do
  d <- letterChar
  x <- read <$> some digitChar
  return (d, x)
