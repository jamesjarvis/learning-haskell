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
-- Complete the 'fizzBuzz' function below.
--
-- The function accepts INTEGER n as parameter.
--

buzzFizz :: Int -> String
buzzFizz n  | n `rem` 15 == 0 = "FizzBuzz"
            | n `rem` 5 == 0 = "Buzz"
            | n `rem` 3 == 0 = "Fizz"
            | otherwise = show n


fizzBuzz n = do
    -- Write your code here
    printAll $ Data.List.map buzzFizz [1..n]
       where
       printAll [] = return ()
       printAll (x:xs) = putStrLn x >> printAll xs

    

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    fizzBuzz n
