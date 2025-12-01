module Main where

import System.Environment (getArgs)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12

main :: IO ()
main = do
    args <- getArgs
    case args of
        [day] -> runDay (read day)
        _     -> putStrLn "Usage: aoc2025 <day>"

runDay :: Int -> IO ()
runDay 1  = Day01.solve
runDay 2  = Day02.solve
runDay 3  = Day03.solve
runDay 4  = Day04.solve
runDay 5  = Day05.solve
runDay 6  = Day06.solve
runDay 7  = Day07.solve
runDay 8  = Day08.solve
runDay 9  = Day09.solve
runDay 10 = Day10.solve
runDay 11 = Day11.solve
runDay 12 = Day12.solve
runDay n  = putStrLn $ "Day " ++ show n ++ " not implemented"
