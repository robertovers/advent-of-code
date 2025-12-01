module Lib where

import Text.Megaparsec
import Data.Void

type Parser = Parsec Void String

readInputFile :: Int -> IO String
readInputFile day = readFile $ "inputs/day" ++ show day ++ ".txt"

readInput :: Int -> IO [String]
readInput day = lines <$> readInputFile day
