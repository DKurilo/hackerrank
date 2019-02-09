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

-- Complete the pairs function below.

data STree a = Tip | Node (STree a) a (STree a)
    deriving (Show)

sinsert :: (Ord a) => a -> STree a -> STree a
sinsert k tree = case tree of
    Tip -> Node Tip k Tip
    Node tl k' tr 
        | k==k' -> Node tl k tr
        | k<k' -> Node (sinsert k tl) k' tr
        | k>k' -> Node tl k' (sinsert k tr)

isInSTree :: (Ord a) => a -> STree a -> Bool
isInSTree k tree = case tree of
    Tip -> False
    Node tl k' tr
        | k==k' -> True
        | k<k' -> isInSTree k tl
        | k>k' -> isInSTree k tr

sfromList :: (Ord a) => [a] -> STree a
sfromList ks = sfromList' ks Tip

sfromList' :: (Ord a) => [a] -> STree a -> STree a
sfromList' [] tree = tree
sfromList' (k:ks) tree = sfromList' ks $ sinsert k tree

pairs :: Int -> [Int] -> Int
pairs k arr = foldl (\a x -> if (isInSTree (x+k) tr) then a+1 else a) 0 arr
    where sarr = DL.sort arr
          tr = sfromList $ (sarr !! ((length arr) `div` 2):arr)

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

    arrTemp <- getLine

    let arr = DL.map (read :: String -> Int) . words $ arrTemp

    let result = pairs k arr

    -- hPutStrLn fptr $ show result
    putStrLn $ show result

    hFlush fptr
    hClose fptr

