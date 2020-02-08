{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe



--
-- Complete the 'arraySum' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts INTEGER_ARRAY numbers as parameter.
--

arraySum :: [Int] -> Int
arraySum [] = 0
arraySum (x:xs) = x + arraySum xs

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    numbersCountTemp <- getLine
    let numbersCount = read $ lstrip $ rstrip numbersCountTemp :: Int

    numbersTemp <- readMultipleLinesAsStringArray numbersCount
    let numbers = Data.List.map (read :: String -> Int) numbersTemp

    let result = arraySum numbers

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
