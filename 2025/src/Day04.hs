module Day04 (solve) where

import Lib

type Coords = (Int, Int)

solve :: IO ()
solve = do
  input <- readInput 4
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input (part1 input))

-- part 1

part1 :: [String] -> Int
part1 = length . accessible

-- part 2

part2 :: [String] -> Int -> Int
part2 mtrx acc = case part1 nxt of
  0 -> acc
  n -> part2 nxt (acc + n)
  where
    nxt = removeAccessible mtrx

removeAccessible :: [String] -> [String]
removeAccessible mtrx =
  [ [ (if (y, x) `elem` accss then '.' else get2d mtrx (y, x))
    | x <- [0 .. length mtrx - 1]
    ]
  | y <- [0 .. length (head mtrx) - 1]
  ]
  where
    accss = accessible mtrx

-- helpers

accessible :: [String] -> [Coords]
accessible mtrx = filter (\c -> numAdjRolls mtrx c < 4) $ filter (\x -> get2d mtrx x == '@') (allIdxs mtrx)

numAdjRolls :: [String] -> Coords -> Int
numAdjRolls mtrx (y, x) = length (filter (== '@') (map (get2d mtrx) (filter (validIdx mtrx) (adjacentIdxs (y, x)))))

adjacentIdxs :: Coords -> [Coords]
adjacentIdxs (y, x) = filter (/= (y, x)) [(y + dy, x + dx) | dy <- [-1 .. 1], dx <- [-1 .. 1]]

allIdxs :: [String] -> [Coords]
allIdxs mtrx = [(y, x) | y <- [0 .. (length mtrx) - 1], x <- [0 .. (length (head mtrx) - 1)]]

validIdx :: [String] -> Coords -> Bool
validIdx mtrx (y, x) = y `elem` [0 .. length mtrx - 1] && x `elem` [0 .. (length (head mtrx) - 1)]

get2d :: [String] -> Coords -> Char
get2d mtrx (y, x) = (mtrx !! y) !! x
