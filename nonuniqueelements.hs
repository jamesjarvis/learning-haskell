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
-- Complete the 'countNonUnique' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts INTEGER_ARRAY numbers as parameter.
--

-- Go through list, add to "visited" list.
-- If list empty, return empty list
-- If not in "visited", then add to "visited" and recurse
-- If in "visited", return itself and recurse
getMultiple :: [Int] -> [Int] -> [Int]
getMultiple [] _    =   []
getMultiple (x:xs) visited  | x `elem` visited = [x] ++ getMultiple xs visited
                            | otherwise = getMultiple xs (visited ++ [x])

removeDuplicates :: [Int] -> [Int] -> [Int]
removeDuplicates [] visited = visited
removeDuplicates (x:xs) visited | x `elem` visited = removeDuplicates xs visited
                                | otherwise = removeDuplicates xs (visited ++ [x])

-- Get all elements that exist more than once in the list
-- Remove duplicates from that list
-- Get the length of that list
countNonUnique :: [Int] -> Int
countNonUnique numbers = Data.List.length (removeDuplicates (getMultiple numbers []) [])
    -- Write your code here

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

    let result = countNonUnique numbers

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
