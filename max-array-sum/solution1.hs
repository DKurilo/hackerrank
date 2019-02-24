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
import qualified Data.Map as DM

-- Complete the maxSubsetSum function below.
maxSubsetSum :: [Int] -> Int
maxSubsetSum [] = 0
maxSubsetSum (a:[]) = a
maxSubsetSum (a1:a2:[]) = max a1 a2
maxSubsetSum (a1:a2:arr) = maxSubsetSum' arr a2 $ DS.singleton a1

maxSubsetSum' :: [Int] -> Int -> DS.Set Int -> Int
maxSubsetSum' [] a' set = max a' $ DS.findMax set
maxSubsetSum' [a] a' set = maximum [a, a + ma, max a' ma]
    where ma = DS.findMax set
maxSubsetSum' (a:as) a' set = maxSubsetSum' as 
                                            (max a (a + (DS.findMax set))) $
                                            DS.insert a' set

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

    arrTemp <- getLine

    let arr = DL.map (read :: String -> Int) . words $ arrTemp

    let res = maxSubsetSum arr

    -- hPutStrLn fptr $ show res
    putStrLn $ show res

    hFlush fptr
    hClose fptr

