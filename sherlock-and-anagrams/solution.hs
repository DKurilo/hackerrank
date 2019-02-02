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

-- Complete the sherlockAndAnagrams function below.
sherlockAndAnagrams :: String -> Int
sherlockAndAnagrams s = sum $ map (comb2from.length) $ findPairs $ ss
    where ss = substrings s

substrings :: String -> [String]
substrings "" = []
substrings (s:ss) = (substrings' (s:ss)) ++ substrings ss

substrings' :: String -> [String]
substrings' s = foldl (\a i -> (DL.sort $ take i s):a) [] [1..length s]

findPairs :: [String] -> [[String]]
findPairs (s:ss) = snd $ foldr (
        \s (s',a) -> (s,if s'==s then ((s:head a):tail a) else ([s]:a))
    ) ("",[]) $ DL.sort $ ss

comb2from :: Int -> Int
comb2from 0 = 0
comb2from 1 = 0
comb2from 2 = 1
comb2from k = (comb2from (k-1)) * k `div` (k-2)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    q <- readLn :: IO Int

    forM_ [1..q] $ \q_itr -> do
        s <- getLine

        let result = sherlockAndAnagrams s

        -- hPutStrLn fptr $ show result
        putStrLn $ show result

    hFlush fptr
    hClose fptr

