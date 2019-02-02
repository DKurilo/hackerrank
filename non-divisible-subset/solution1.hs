{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import qualified Data.Map.Strict as DM
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the nonDivisibleSubset function below.
nonDivisibleSubset :: Int -> [Int] -> Int
nonDivisibleSubset k ns = fst $ nonDivisibleSubset' k ns DM.empty

nonDivisibleSubset' :: Int -> [Int] -> DM.Map [Int] Int -> (Int, DM.Map [Int] Int)
nonDivisibleSubset' k [] m = (0, m)
nonDivisibleSubset' k [n1,n2] m | n1 + n2 `mod` k == 0 = (0,m)
                                | otherwise = (2,m)
nonDivisibleSubset' k ns m = case DM.lookup ns m of
    Just c -> (c, m)
    _ -> Prelude.foldr (\i (c',m') -> 
            let (c'',m'') = nonDivisibleSubset' k ns' m'
                ns' = Prelude.filter (\x -> x/=i && (x+i) `mod` k /= 0) ns 
            in (max c' (1+c''),DM.insert ns' c'' m'')) (0,m) ns

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

