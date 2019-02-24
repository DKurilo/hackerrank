{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the candies function below.

data State = Rise | Rise2 | Fall Int | Stable deriving (Show, Eq)

candies :: [Int] -> Int
candies [] = 0
candies (a:[]) = 1
candies (a1:a2:as)
    | a1 > a2 = candies' as [a2, a1] (Fall 1) 0
    | a1 < a2 = candies' as [a2, a1] Rise 0
    | a1 == a2 = candies' as [a2, a1] Stable 0

candies' :: [Int] -> [Int] -> State -> Int -> Int
candies' [] bs st c = case st of
    Rise -> c + l * (l + 1) `div` 2
    Rise2 -> c + l + l * (l + 1) `div` 2
    Fall m -> c + l * (l + 1) `div` 2 + (if m > l then (m - l) else 0)
    Stable -> c + l
    where l = length bs
candies' (a:as) bs st c = case st of
    Rise   | a > head bs -> candies' as (a:bs) Rise c
           | a == head bs -> candies' as [a] Stable $ c + l * (l + 1) `div` 2
           | otherwise -> candies' as [a, head bs] (Fall l) $ c + l * (l - 1) `div` 2
    Rise2  | a > head bs -> candies' as (a:bs) Rise2 c
           | a == head bs -> candies' as [a] Stable $ c + l + l * (l + 1) `div` 2
           | otherwise -> candies' as [a, head bs] (Fall $ l + 1) $ 
                                   c + l - 1 + l * (l - 1) `div` 2
    Stable | a > head bs -> candies' as [a, head bs] Rise $ c + l - 1
           | a == head bs -> candies' as (a:bs) Stable $ c
           | otherwise -> candies' as [a, head bs] (Fall 1) $ c + l - 1
    Fall m | a > head bs -> candies' as [a] Rise2 $ c + l * (l + 1) `div` 2 + 
               (if m > l then (m - l) else 0)
           | a == head bs -> candies' as [a] Stable $ c + l * (l + 1) `div` 2 + 
               (if m > l then (m - l) else 0)
           | otherwise -> candies' as (a:bs) (Fall m) c
    where l = length bs

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

    n <- readLn :: IO Int

    arrTemp <- readMultipleLinesAsStringArray n
    let arr = DL.map (read :: String -> Int) arrTemp

    let result = candies arr

    -- hPutStrLn fptr $ show result
    putStrLn $ show result

    hFlush fptr
    hClose fptr

