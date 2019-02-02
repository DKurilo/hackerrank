{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import qualified Data.Set as DS
import qualified Data.Map.Strict as DM
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

import Debug.Trace

-- Complete the nonDivisibleSubset function below.
nonDivisibleSubset :: Int -> [Int] -> Int
nonDivisibleSubset k ns = maximum $ Prelude.map length $ nonDivisibleSubset' k ns $ trace (show $ q) $ q where q =  createCache k ns DM.empty

nonDivisibleSubset' :: Int -> [Int] -> DM.Map Int [Int] -> [[Int]]
nonDivisibleSubset' k ns m = Prelude.foldr (\n a-> case DM.lookup n m of
        Just ns' -> if ins == [] then [n]:a else (Prelude.map (\ns'' -> n:ns'') $ nonDivisibleSubset' k ins m) ++ a
            where ins = getIntersection ns' ns
        _ ->[n]:a) [] ns

createCache :: Int -> [Int] -> DM.Map Int [Int] -> DM.Map Int [Int]
createCache _ (_:[]) m = m
createCache k (n:ns) m = DM.insert n (Prelude.filter (\n' -> (n+n') `mod` k /= 0) ns) $ createCache k ns m

getIntersection :: [Int] -> [Int] -> [Int]
getIntersection ns ms = DS.toList $ DS.intersection (DS.fromList ns) (DS.fromList ms)

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

    nkTemp <- getLine
    let nk = words nkTemp

    let n = read (nk !! 0) :: Int

    let k = read (nk !! 1) :: Int

    stemp <- getLine

    let s = Data.List.map (read :: String -> Int) . words $ stemp

    let result = nonDivisibleSubset k s

    putStrLn . show $ result
    -- hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr

