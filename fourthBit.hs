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
-- Complete the 'fourthBit' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts INTEGER number as parameter.
--

-- intToBinary should convert an int to a list of ints, the binary representation
intToBinary :: Int -> [Int]
intToBinary 0 = []
intToBinary n = [n `mod` 2] ++ intToBinary (n `div` 2)


fourthBit number = (intToBinary number) !! 3


lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    numberTemp <- getLine
    let number = read $ lstrip $ rstrip numberTemp :: Int

    let result = fourthBit number

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
