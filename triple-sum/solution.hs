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

-- Complete the triplets function below.

data STree a = Tip | Node Int (STree a) a Int (STree a)
    deriving (Show)

countSmaller :: (Ord a) => a -> STree a -> Int
countSmaller k tree = case tree of
    Tip -> 0
    Node cl tl k' cr tr
        | k>k' -> 1 + cl + countSmaller k tr
        | k==k' -> 1 + cl
        | k<k' -> countSmaller k tl

sfromList :: (Ord a) => [a] -> STree a
sfromList [] = Tip
sfromList ks = 
    Node (length kls - 1) 
         (sfromList . reverse . tail $ kls) 
         (head kls) 
         (length krs)
         (sfromList krs)
    where (kls, krs) = halves ks

triplets :: [Int] -> [Int] -> [Int] -> Int
triplets a b c = sum $ q
    where tra = sfromList . distinct $ a 
          trc = sfromList . distinct $ c
          q = map (\x -> countSmaller x tra * countSmaller x trc) $ distinct b

distinct :: [Int] -> [Int]
distinct = fst .
           foldr (\x (xs,x')-> if x==x' then (xs,x) else ((x:xs),x) ) ([], 0) . DL.sort 

halves :: [a] -> ([a], [a])
halves xs = halves' xs xs [] 0

halves' :: [a] -> [a] -> [a] -> Int -> ([a],[a])
halves' [] [] bs _ = (bs,[])
halves' [] ss bs _ = (bs,ss)
halves' (f:fs) (s:ss) bs c
    | c `mod` 2 == 0 = halves' fs ss (s:bs) $ c+1
    | otherwise = halves' fs (s:ss) bs $ c+1

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

    lenaLenbLencTemp <- getLine
    let lenaLenbLenc = words lenaLenbLencTemp

    let lena = read (lenaLenbLenc !! 0) :: Int

    let lenb = read (lenaLenbLenc !! 1) :: Int

    let lenc = read (lenaLenbLenc !! 2) :: Int

    arraTemp <- getLine

    let arra = DL.map (read :: String -> Int) . words $ arraTemp

    arrbTemp <- getLine

    let arrb = DL.map (read :: String -> Int) . words $ arrbTemp

    arrcTemp <- getLine

    let arrc = DL.map (read :: String -> Int) . words $ arrcTemp

    let ans = triplets arra arrb arrc

    -- hPutStrLn fptr $ show ans
    putStrLn $ show ans

    hFlush fptr
    hClose fptr

