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
nonDivisibleSubset k ns = sum $ getNs k [0..k `div` 2] $ getModulos k ns

getNs :: Int -> [Int] -> DM.Map Int Int -> [Int]
getNs _ [] _ = []
getNs k (n:ns) ms | (n == 0 || n*2 == k) && DM.lookup n ms /= Just 0 = (1:getNs k ns ms)
                  | otherwise = (max n1 n2: getNs k ns ms)
                      where n1 = case DM.lookup n ms of Just i -> i
                                                        _ -> 0
                            n2 = case DM.lookup (k-n) ms of Just i -> i
                                                            _ -> 0

getModulos :: Int -> [Int] -> DM.Map Int Int
getModulos k ns = Prelude.foldr (\n m -> DM.update (\v -> Just $ v+1) (n `mod` k) m) (DM.fromList $ zip [0..k-1] [0,0..]) ns

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

