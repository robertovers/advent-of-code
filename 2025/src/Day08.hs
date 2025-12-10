module Day08 (solve) where

import Lib
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List
import Debug.Trace
import Data.Function (on)
import Data.Map (Map, fromList, toList, (!), delete, insert, update, alter)

debug = flip trace

type Box = (Float, Float, Float)
type Circuit = [Box]

solve :: IO ()
solve = do
    input <- readInput 8
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

-- part 1

part1 :: [String] -> Int
part1 input = case traverse (parse parseBox "") input of
  Left _ -> error "bad input" `debug` show input
  Right x -> (-1) * product (take 3 $ sort $ map ((-1) *) lengths)
    where
      circuits = fromList $ zip [0..length x-1] (map (: []) x)
      endCircuits = foldr (uncurry connect) circuits (reverse $ take 1000 (sortedPairs x))
      lengths = map (length . snd) $ toList endCircuits

-- part 2

-- i just printed out the connections
part2 :: [String] -> Int
part2 input = case traverse (parse parseBox "") input of
  Left _ -> error "bad input" `debug` show input
  Right x -> 0
    where
      circuits = fromList $ zip [0..length x-1] (map (: []) x)
      endCircuits = foldr (uncurry connect) circuits (reverse $ sortedPairs x)

-- helpers

connect :: Box -> Box -> Map Int Circuit -> Map Int Circuit
connect a b circuits = if an == bn
  then circuits
  else ((Data.Map.alter connected an) . (Data.Map.delete bn)) circuits `debug` show (a,b)
  where
    an = getCircuit a (toList circuits)
    bn = getCircuit b (toList circuits)
    connected _ = Just $ (circuits ! an) ++ (circuits ! bn)

getCircuit :: Box -> [(Int, Circuit)] -> Int
getCircuit b [] = error "not found"
getCircuit b ((cn, cs):rest) =
  if b `elem` cs then cn else getCircuit b rest

sortedPairs :: [Box] -> [(Box, Box)]
sortedPairs boxes = sortBy (compare `on` uncurry distance) [(a, b) | (a:bs) <- tails boxes, b <- bs]

distance :: Box -> Box -> Float
distance (ax, ay, az) (bx, by, bz) =
  sqrt $ (ax - bx) ^ 2 + (ay - by) ^ 2 + (az - bz) ^ 2

parseBox :: Parser Box
parseBox = do
  x <- read <$> some digitChar
  _ <- char ','
  y <- read <$> some digitChar
  _ <- char ','
  z <- read <$> some digitChar
  return (x, y, z)
