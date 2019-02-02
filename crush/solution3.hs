{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Bits
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import Data.Foldable
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import Data.Array.IO

import Debug.Trace

-- Complete the arrayManipulation function below.
arrayManipulation :: Int -> [[Int]] -> IO Int
arrayManipulation n queries = do
        arr <- (newArray (1,n+1) 0) :: IO (IOUArray Int Int)
        mapM_ (\(f:t:v:_) -> do
                       vf <- readArray arr (n+2-f)
                       writeArray arr (n+2-f) (vf+v)
                       vt <- readArray arr (n+1-t)
                       writeArray arr (n+1-t) (vt-v)
              ) queries

        (max,_) <- foldrM (\i (m,v) -> do
                d <- readArray arr i
                let v' = v + d
                return (max m v', v')
            ) (0,0) [2..n+1]
        return $ max

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

    nmTemp <- getLine
    let nm = words nmTemp

    let n = read (nm !! 0) :: Int

    let m = read (nm !! 1) :: Int

    queriesTemp <- readMultipleLinesAsStringArray m
    let queries = DL.map (\x -> DL.map (read :: String -> Int) . words $ x) queriesTemp

    result <- arrayManipulation n queries

    -- hPutStrLn fptr $ show result
    putStrLn $ show result

    hFlush fptr
    hClose fptr


