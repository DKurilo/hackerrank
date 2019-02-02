{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import System.Environment
import System.IO
import Debug.Trace

--
-- Complete the liars function below.
--
liars :: Int -> [[Int]] -> [Int]
liars n sets = [l,n-g]
    where l = case remains n [1..n] 0 sets of Just k -> k
                                              _ -> n+1 -- error
          g = case (remains n [1..n] 0 $ inv sets) of Just k -> k
                                                      _ -> n+1 -- error

inv :: [[Int]] -> [[Int]]
inv = map (\(s:f:q:_)->[s,f,f-s-q+1])

remains :: Int -> [Int] -> Int -> [[Int]] -> Maybe Int
remains n [] g _ = Just (n-g)
remains n ss g sets = 
    case mp of
        Just p -> case mrs p of
                      Just rs -> Just rs
                      Nothing -> mrs' p
        Nothing | setsEmpty -> Just (n - g - length ss)
                | otherwise -> Nothing
    where mp = findPosition sets ss
          setsEmpty = and $ map (\(_:_:q:_) -> q == 0) $ sets
          mrs p = case excludeSome True p ss sets 0 of
                      Just (ss',sets',g') -> remains n ss' (g+g') sets'
                      Nothing -> Nothing
          mrs' p  = case excludeSome False p ss sets 0 of
                        Just (ss',sets',g') -> remains n ss' (g+1+g') sets'
                        Nothing -> Nothing

findPosition :: [[Int]] -> [Int] -> Maybe Int
findPosition sets ss 
    | m <= 0 = Nothing
    | otherwise = Just sp
    where (m,sp) = maximum $ 
            map 
                (\s->(foldl (\a (b:f:q:_)-> if b<=s && f>=s then 
                        if q>0 && (length $ filter (\s -> b<=s && f>=s) ss) >= q then a+1
                        else (-200)
                    else a) 
                0 sets,s)) ss

excludeSome :: Bool -> Int -> [Int] -> [[Int]] -> Int -> Maybe ([Int],[[Int]],Int)
excludeSome isLiar p ss sets g = trace (show isLiar ++ " " ++ show p ++ " " ++ show ss ++ "->" ++ show ss' ++ " sets " ++ show sets ++ "->" ++ show sets' ) $
    if emptySets /= [] then 
        excludeSome False 
                    (head $ filter(\p -> p>=(head $ head emptySets)) ss') 
                    ss' sets' (g+1)
    else if fullSets /= [] then
        excludeSome True 
                    (head $ filter(\p -> p>=(head $ head fullSets)) ss') 
                    ss' sets' g
    else if wrongSets == [] then Just (ss', sets', g)
    else Nothing
    where sets' = if isLiar then excludeSt p sets else sets
          ss' = excludeSl p ss
          emptySets = filter (\(b:f:q:_) -> q==0 && (soldiersInSet ss' [b,f]) /= []) sets'
          fullSets = filter (\(b:f:q:_) -> 
                  q > 0 && (length $ soldiersInSet ss' [b,f]) == q) sets'
          wrongSets = filter (\(b:f:q:_) -> q < 0 ||
                                 (length $ soldiersInSet ss' [b,f]) < q) sets'

excludeSt :: Int -> [[Int]] -> [[Int]]
excludeSt p sets = 
    foldl (\a (s:f:q:_)-> if s<=p && f>=p then ([s,f,q-1]:a) else ([s,f,q]:a)) [] sets

excludeSl :: Int -> [Int] -> [Int]
excludeSl p ss = filter (/=p) ss

soldiersInSet :: [Int] -> [Int] -> [Int]
soldiersInSet ss (b:f:_)  = filter (\p -> p>=b && p<=f) ss

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

    nmTemp <- getLine
    let nm = words nmTemp

    let n = read (nm !! 0) :: Int

    let m = read (nm !! 1) :: Int

    setsTemp <- readMultipleLinesAsStringArray m
    let sets = DL.map (\x -> DL.map (read :: String -> Int) . words $ x) setsTemp

    let result = liars n sets

    hPutStrLn fptr $ DL.intercalate " " $ DL.map (\x -> show x) $ result
    -- putStrLn $ DL.intercalate " " $ DL.map (\x -> show x) $ result

    hFlush fptr
    hClose fptr

