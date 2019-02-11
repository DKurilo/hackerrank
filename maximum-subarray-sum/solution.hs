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

-- Complete the maximumSum function below.

maximumSum :: [Int] -> Int -> Int
maximumSum a m = maximum $ (\(a,_,_) -> a) $ foldr (getnextS m) ([], 0, DS.empty) a

getnextS :: Int -> Int -> ([Int], Int, DS.Set Int) -> ([Int], Int, DS.Set Int)
getnextS m x' (ss, x, sss) = ([s'] ++ mins ++ ss, s', sss')
    where s' = (x + x') `mod` m
          mins = case DS.lookupGT s' sss of Just a -> [(s'-a) `mod` m]
                                            Nothing -> []
          sss' = DS.insert s' sss

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

    q <- readLn :: IO Int

    forM_ [1..q] $ \q_itr -> do
        nmTemp <- getLine
        let nm = words nmTemp

        let n = read (nm !! 0) :: Int

        let m = read (nm !! 1) :: Int

        aTemp <- getLine

        let a = DL.map (read :: String -> Int) . words $ aTemp

        let result = maximumSum a m

        -- hPutStrLn fptr $ show result
        putStrLn $ show result

    hFlush fptr
    hClose fptr

