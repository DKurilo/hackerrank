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
    where l = case remains n [1..n] 0 sets [] of Just k -> k
                                                 _ -> n+1 -- error
          g = case (remains n [1..n] 0 (inv sets) []) of Just k -> k
                                                         _ -> n+1 -- error

inv :: [[Int]] -> [[Int]]
inv = map (\(s:f:q:_)->[s,f,f-s-q+1])

remains :: Int -> [Int] -> Int -> [[Int]] -> [[Int]] -> Maybe Int
remains n [] g _ _ = Just (n-g)
remains n ss g sets excl = 
    case mp of
        Just p -> case mrs p of
                      Just rs -> Just rs
                      Nothing -> mrs' p
        Nothing | setsEmpty -> Just (n - g - length ss)
                | otherwise -> Nothing
    where (mp,excl') = findPosition sets ss excl
          setsEmpty = and $ map (\(_:_:q:_) -> q == 0) $ sets
          mrs p = case excludeSome True True p ss sets g of
                      Just (ss',sets',g') -> remains n ss' g' sets' []
                      Nothing -> Nothing
          mrs' p  = case excludeSome False True p ss sets g of
                        Just (ss',sets',g') -> remains n ss' g' sets' excl'
                        Nothing -> Nothing

findPosition :: [[Int]] -> [Int] -> [[Int]] -> (Maybe Int, [[Int]])
findPosition sets ss excl
    | m <= 0 = (Nothing, [])
    | otherwise = (Just sp, setsForSoldier sp sets)
    where (m,sp) = maximum $ -- trace (show excl) $
            filter (\(_,s) -> excl==[] || 
                    (mt2 $ setsForSoldier s sets) /= (mt2 excl)) $
            map 
                (\s->(foldl (\a (b:f:q:_)-> if b<=s && f>=s then 
                        if q>0 && (length $ soldiersInSet ss [b,f]) >= q then a+1
                        else (-200)
                    else a) 
                0 sets,s)) ss

excludeSome :: Bool -> Bool -> Int -> [Int] -> [[Int]] -> Int -> Maybe ([Int],[[Int]],Int)
excludeSome isLiar ext p ss sets g =
    if emptySets /= [] then
        excludeSome False False
                    (head $ filter(\p -> p>=(head $ head emptySets)) ss') 
                    ss' sets' g'
    else if fullSets /= [] then 
        excludeSome True False
                    (head $ filter(\p -> p>=(head $ head fullSets)) ss') 
                    ss' sets' g'
    else if wrongSets == [] then Just (ss', sets', g')
    else Nothing
    where sets' = if isLiar then excludeSt p sets else sets
          (ss',g') = if isLiar then (excludeSl p ss,g) else if ext then
                        (\(ss'',g'') -> (reverse ss'', g'')) $ foldl (\(ss'',g'') s -> 
                               if s==p || (mt2 $ setsForSoldier s sets) == psets then
                                   (ss'',g''+1)
                               else ((s:ss''),g'')
                                 ) ([],g) ss
                      else (excludeSl p ss,g+1)
          psets = mt2 $ setsForSoldier p sets
          emptySets = filter (\(b:f:q:_) -> q==0 && (soldiersInSet ss' [b,f]) /= []) sets'
          fullSets = filter (\(b:f:q:_) -> 
                  q > 0 && (length $ soldiersInSet ss' [b,f]) == q) sets'
          wrongSets = filter (\(b:f:q:_) -> q < 0 ||
                                 (length $ soldiersInSet ss' [b,f]) < q) sets'

excludeSt :: Int -> [[Int]] -> [[Int]]
excludeSt p sets = reverse $ 
    foldl (\a (b:f:q:_)-> if b<=p && f>=p then ([b,f,q-1]:a) else ([b,f,q]:a)) [] sets

excludeSl :: Int -> [Int] -> [Int]
excludeSl p ss = filter (/=p) ss

soldiersInSet :: [Int] -> [Int] -> [Int]
soldiersInSet ss (b:f:_)  = filter (\p -> p>=b && p<=f) ss

setsForSoldier :: Int -> [[Int]] -> [[Int]]
setsForSoldier s sets = filter (\(b:f:q:_) -> s>=b && s<=f && q>0) sets

mt2 :: [[a]] -> [[a]]
mt2 = map (take 2)
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

    -- hPutStrLn fptr $ DL.intercalate " " $ DL.map (\x -> show x) $ result
    putStrLn $ DL.intercalate " " $ DL.map (\x -> show x) $ result

    hFlush fptr
    hClose fptr

