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
import Data.Array.IO

import Debug.Trace

-- Complete the minimumSwaps function below.
minimumSwaps :: [Int] -> IO Int
minimumSwaps arr = do
    let l = length arr
    a' <- newListArray (1,l) arr
    minimumSwaps' 1 l a'

minimumSwaps' :: Int -> Int -> IOUArray Int Int -> IO Int
minimumSwaps' i l ns = do
    if i>l then return 0 else do
        v <- readArray ns i
        if  v == i then minimumSwaps' (i+1) l ns
        else do 
            v' <- readArray ns v
            writeArray ns i v'
            writeArray ns v v
            ms <- minimumSwaps' i l ns
            return $ ms + 1

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- readLn :: IO Int

    arrTemp <- getLine

    let arr = DL.map (read :: String -> Int) . words $ arrTemp

    res <- minimumSwaps arr

    putStrLn . show $ res
    -- hPutStrLn fptr $ show res

    hFlush fptr
    hClose fptr

