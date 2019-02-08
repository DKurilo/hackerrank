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

-- I can do it with Data.Map. It's really easy and fast, but I want to build my own binary tree. Just to try it in Haskell
-- Complete the whatFlavors function below.


data STree a b = Tip | Node (STree a b) a [b] (STree a b)
    deriving (Show)

sinsert :: (Ord a) => a -> b -> STree a b -> STree a b
sinsert k v tree = case tree of
    Tip -> Node Tip k [v] Tip
    Node tl k' vs tr 
        | k==k' -> Node tl k (v:vs) tr
        | k<k' -> Node (sinsert k v tl) k' vs tr
        | k>k' -> Node tl k' vs (sinsert k v tr)

slookup :: (Ord a) => a -> STree a b -> [b]
slookup k tree = case tree of
    Tip -> []
    Node tl k' vs tr
        | k==k' -> vs
        | k<k' -> slookup k tl
        | k>k' -> slookup k tr

sfromList :: (Ord a) => [(a,b)] -> STree a b
sfromList kvs = sfromList' kvs Tip

sfromList' :: (Ord a) => [(a,b)] -> STree a b -> STree a b
sfromList' [] tree = tree
sfromList' ((k,v):kvs) tree = sfromList' kvs $ sinsert k v tree

whatFlavors :: [Int] -> Int -> (Int, Int)
whatFlavors cost money = whatFlavors' zcs money tree
    where zcs = zip cost [1..]
          mcs = (0,(minimum cost + maximum cost) `div` 2)
          tree = sfromList (mcs:zcs)

whatFlavors' :: [(Int,Int)] -> Int -> STree Int Int -> (Int, Int)
whatFlavors' [] _ _ = (0,0) -- error
whatFlavors' ((c,i):zcs) m tr
    | cs == [] = whatFlavors' zcs m tr
    | otherwise = (i, head cs)
    where cs = filter (\k -> k/=0 && k/=i) $ slookup (m-c) tr

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    t <- readLn :: IO Int

    forM_ [1..t] $ \t_itr -> do
        money <- readLn :: IO Int

        n <- readLn :: IO Int

        costTemp <- getLine

        let cost = DL.map (read :: String -> Int) . words $ costTemp

        putStrLn $ (\(p1,p2) -> show p1 ++ " " ++ show p2) $ whatFlavors cost money

