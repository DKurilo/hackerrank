{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Bits
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import Data.Array.ST

import Debug.Trace

-- Complete the arrayManipulation function below.
arrayManipulation :: Int -> [[Int]] -> Int
arrayManipulation n queries = case mm of
    (Just n) -> n
    _ -> 0
    where mm = (\(mm',_,_) -> mm') $ 
              foldr processQuery (Nothing,0,[]) $ 
              DL.sortBy (\(a:_) (b:_) -> compare b a) queries

processQuery :: [Int] -> (Maybe Int, Int, [(Int, Int, Int)]) -> (Maybe Int, Int, [(Int, Int, Int)])
processQuery (f:t:v:_) (mm, s, cqs) = (
        case mm of 
            (Just m) -> Just $ max m cv'
            Nothing -> Just cv', cv', scqs
    )
    where cv' = cv+v
          scqs =  DL.sortBy (\(_,a,_) (_,b,_) -> compare a b) ((f,t,v):cqs')
          (cv, cqs') = removeOld f s cqs

removeOld :: Int -> Int -> [(Int,Int,Int)] -> (Int,[(Int,Int,Int)])
removeOld _ v [] = (v, [])
removeOld f v ((f',t',v'):qs)
    | f > t' = removeOld f (v-v') qs
    | otherwise = (v,((f',t',v'):qs))

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

    queriesTemp <- readMultipleLinesAsStringArray m
    let queries = DL.map (\x -> DL.map (read :: String -> Int) . words $ x) queriesTemp

    let result = arrayManipulation n queries

    -- hPutStrLn fptr $ show result
    putStrLn $ show result

    hFlush fptr
    hClose fptr


