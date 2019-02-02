{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import qualified Data.Set as DS
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the activityNotifications function below.
activityNotifications :: [Int] -> Int -> Int
activityNotifications expenditure d = activityNotifications' d us es $ DS.fromList cs
    where es = zip expenditure [1..]
          (cs,us) = splitAt d es

activityNotifications' :: Int -> [(Int,Int)] -> [(Int,Int)] -> DS.Set (Int,Int) -> Int
activityNotifications' _ [] _ _ = 0
activityNotifications' d ((e,i):us) es fes = 
    (if e >= median2 then 1 else 0) + (activityNotifications' d us (tail es) $
        DS.insert (e,i) $ DS.delete last fes)
    where 
        last = head $ es
        median2 = ((fst $ DS.elemAt (d `div` 2) fes) + (fst $ DS.elemAt ((d `div` 2) - ((d+1) `mod` 2)) fes))

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

    ndTemp <- getLine
    let nd = words ndTemp

    let n = read (nd !! 0) :: Int

    let d = read (nd !! 1) :: Int

    expenditureTemp <- getLine

    let expenditure = Data.List.map (read :: String -> Int) . words $ expenditureTemp

    let result = activityNotifications expenditure d

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr

