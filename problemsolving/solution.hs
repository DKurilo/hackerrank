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
data State = ST { stps :: [Int]
                , stday :: Int
                , stboard :: [Int]
                } | EMPTY

problemSolving :: Int -> [Int] -> Int
problemSolving _ [] = 0
problemSolving k vs = problemSolving' k vs 1 EMPTY

problemSolving' :: Int -> [Int] -> Int -> State -> Int
problemSolving' k vs days EMPTY | fst try = days
                                | otherwise = problemSolving' k (stps st) (days + 1) $ ST (stps st) (stday st) $ stboard st ++ [0]
    where try = tryFill k vs days
          st = snd try
problemSolving' k vs days (ST svs sday sboard) 
    | fst try = days
    | otherwise = problemSolving' k (stps st) (days + 1) $ ST (stps st) (stday st) $ stboard st ++ [0]
    where try = tryFill' k svs days sday sboard
          st = snd try

tryFill :: Int -> [Int] -> Int -> (Bool, State)
tryFill k vs days = tryFill' k vs days 0 $ Prelude.take days $ repeat 0

tryFill' :: Int -> [Int] -> Int -> Int -> [Int] -> (Bool, State)
tryFill' _ [] _ _ _ = (True, EMPTY)
tryFill' k (v:vs) days day board 
    | day >= days = (False, ST (v:vs) day board)
    | onboard == 0 = tryFill' k vs days 0 (replaceEl board day v)
    | canplace && (fst $ tryFill' k vs days 0 (replaceEl board day v)) = (True, EMPTY)
    | otherwise = tryFill' k (v:vs) days (day+1) board
    where canplace = abs (onboard - v) >= k
          onboard = board!!day
 
replaceEl :: [a] -> Int -> a -> [a]
replaceEl xs i val = replaceEl' xs i val 0

replaceEl' :: [a] -> Int -> a -> Int -> [a]
replaceEl' [] _ _ _ = []
replaceEl' (x:xs) i val ci | i == ci = (val:xs)
                                | otherwise = (x:replaceEl' xs i val (ci+1))
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
        putStrLn $ show result

        return ()

    hFlush fptr
    hClose fptr

