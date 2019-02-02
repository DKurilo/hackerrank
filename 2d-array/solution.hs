{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

import Debug.Trace

-- Complete the hourglassSum function below.
hourglassSum :: [[Int]] -> Int
hourglassSum arr = maximum $ Prelude.map (sum.(applyPattern [1,1,1,0,1,0,1,1,1])) $ trace (show q) $ q where q = breakSq 3 3 arr

applyPattern :: Num a => [a] -> [a] -> [a]
applyPattern ps = Prelude.map (\(x,y) -> x*y) . zip ps

breakSq :: Show a => Int -> Int -> [[a]] -> [[a]]
breakSq w h as = breakSq' w h [] [] as

breakSq' :: Show a => Int -> Int -> [[a]] -> [[[a]]] -> [[a]] -> [[a]]
breakSq' _ _ rs [] [] = rs -- error
breakSq' _ _ rs (c:cs) [] = rs ++ c
breakSq' w h rs cs (a:as) 
    | length cs >= h = breakSq' w h (rs ++ head cs) (applyLine . tail $ cs) as
    | otherwise = breakSq' w h rs (applyLine cs) as
        where sw = length a - w + 1
              applyLine = \cs' -> Prelude.map (\l -> zipWith (++) l pa) $ cs' ++ [Prelude.take sw $ repeat []]
              pa = reverse $ fst $ Prelude.foldr (\n (ls,(ns)) -> (Prelude.take w ns:ls, tail ns)) ([],a) [1..sw]

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

    arrTemp <- readMultipleLinesAsStringArray 6
    let arr = Data.List.map (\x -> Data.List.map (read :: String -> Int) . words $ x) arrTemp

    let result = hourglassSum arr

    -- hPutStrLn fptr $ show result
    putStrLn $ show result

    hFlush fptr
    hClose fptr

