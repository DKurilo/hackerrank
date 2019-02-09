{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import System.Environment
import System.IO

--
-- Complete the swapNodes function below.
--

data BTree a = Tip | Node (BTree a) a Int (BTree a)
    deriving (Show)

bfill :: [[Int]] -> BTree Int
bfill = bfill' 1 1

bfill' :: Int -> Int -> [[Int]] -> BTree Int
bfill' (-1) _ _ = Tip
bfill' i d ns = Node (bfill' il (d+1) ns) i d (bfill' ir (d+1) ns)
    where (il,ir) = (\(a:b:_) -> (a,b)) $ ns !! (i-1)

btraverse :: BTree a -> [a]
btraverse Tip = []
btraverse (Node trl k _ trr) = btraverse trl ++ [k] ++ btraverse trr

bswap :: Int -> BTree a -> BTree a
bswap _ Tip = Tip
bswap d' (Node trl k d trr)
    | d `mod` d' == 0 = Node (bswap d' trr) k d (bswap d' trl)
    | otherwise = Node (bswap d' trl) k d (bswap d' trr)

swapNodes :: [[Int]] -> [Int] -> [[Int]]
swapNodes indexes queries = reverse . fst $ foldl perform ([],bfill indexes) queries
    where perform (rs, tr) q = let swtr = bswap q tr in (btraverse swtr:rs, swtr)

    --
    -- Write your code here.
    --

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

    indexesTemp <- readMultipleLinesAsStringArray n
    let indexes = DL.map (\x -> DL.map (read :: String -> Int) . words $ x) indexesTemp

    queriesCount <- readLn :: IO Int

    queriesTemp <- readMultipleLinesAsStringArray queriesCount
    let queries = DL.map (read :: String -> Int) queriesTemp

    let result = swapNodes indexes queries

    -- hPutStrLn fptr $ DL.intercalate "\n" $ DL.map (\x -> DL.intercalate " " $ DL.map (\y -> show y) $ x) $ result
    putStrLn $ DL.intercalate "\n" $ DL.map (\x -> DL.intercalate " " $ DL.map (\y -> show y) $ x) $ result

    hFlush fptr
    hClose fptr

