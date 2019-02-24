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
import qualified Data.Map as DM

-- Complete the maxSubsetSum function below.
maxSubsetSum :: [Int] -> Int
maxSubsetSum arr = maxSubsetSum' arr $ DM.fromListWith (++) $ zip arr $ map (:[]) [0..]

maxSubsetSum' :: [Int] -> DM.Map Int [Int] -> Int
maxSubsetSum' arr map = if as == [] then maximum arr else sum as 
    where as = (\(xs,_,_)->xs) $ foldl addMax ([],DS.empty,map) arr

addMax :: ([Int], DS.Set Int, DM.Map Int [Int]) -> a -> ([Int], DS.Set Int, DM.Map Int [Int])
addMax (xs, iset, nmap) _ = if mxi < 0 DS.member (mxi+1) iset || DS.member (mxi-1) iset
                            then (xs, iset, nmap')
                            else ((mx:xs), DS.insert mxi iset, nmap')
    where (mx, mxi, nmap') = popMax nmap

popMax :: DM.Map Int [Int] -> (Int, Int, DM.Map Int [Int])
popMax map = case DM.lookupMax map of
    Just (v,is) -> (v,head is, DM.updateMax (
            \is' -> if tail is' == [] then Nothing else (Just $ tail is')) map)
    _ -> (0, 0, map) -- already empty

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

    let res = maxSubsetSum arr

    -- hPutStrLn fptr $ show res
    putStrLn $ show res

    hFlush fptr
    hClose fptr

