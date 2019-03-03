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

-- Complete the riddle function below.
type Min = Int
type Max= Int
type Last = Int
data Window = W Last Min

riddle :: [Int] -> [Int]
riddle arr = riddle' . map (\n -> W n n) $ arr

riddle' :: [Window] -> [Int]
riddle' (W l m:[]) = [m]
riddle' ws = (mx:riddle' ws')
    where (mx,ws') = next ws

next :: [Window] -> (Max,[Window])
next [] = (0,[])
next (W _ m:[]) = (m,[])
next (W _ m':W l m'':ws) = (max m' mx, (W l m:ws'))
    where m = min m' m''
          (mx, ws') = next (W l m'':ws)

-- complete this function

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

    let res = riddle arr

    hPutStrLn fptr $ DL.intercalate " " $ DL.map (\x -> show x) $ res

    hFlush fptr
    hClose fptr

