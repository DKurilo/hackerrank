{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.List
import Data.List.Split
import Data.Set
import System.Environment
import System.IO

--
-- Complete the problemSolving function below.
--
problemSolving :: Int -> [Int] -> Int
problemSolving k ps = problemSolving' k ps 1

problemSolving' :: Int -> [Int] -> Int -> Int
problemSolving' _ [] d = d
problemSolving' k (vi:vis) d
    | days > d = problemSolving' k vis days
    | otherwise = problemSolving' k vis d
    where days = getDays k (vi:vis)

getDays :: Int -> [Int] -> Int
getDays _ [] = 0
getDays k (vi:vis) = getDays' k vis 1 vi

getDays' :: Int -> [Int] -> Int -> Int -> Int
getDays' _ [] d _ = d
getDays' k (vi:vis) d s 
    | abs (vi - s) < k = getDays' k vis (d+1) s
    | otherwise = d

--
-- Write your code here.
--

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

    t <- readLn :: IO Int

    replicateM_ t $ do
        nkTemp <- getLine
        let nk = words nkTemp

        let n = read (nk !! 0) :: Int

        let k = read (nk !! 1) :: Int

        vTemp <- getLine

        let v = Data.List.map (read :: String -> Int) . words $ vTemp

        let result = problemSolving k v

        -- hPutStrLn fptr $ show result
        putStrLn . show $ result

        return ()

    hFlush fptr
    hClose fptr

