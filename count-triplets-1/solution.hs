{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import qualified Data.Text as DT
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import qualified Data.Map as DM

-- Complete the countTriplets function below.
countTriplets :: [Integer] -> Integer -> Integer
countTriplets arr r = countTriplets' arr r DM.empty DM.empty

countTriplets' :: [Integer] -> Integer -> DM.Map Integer Integer -> DM.Map Integer Integer -> Integer
countTriplets' [] _ _ _ = 0
countTriplets' (x:xs) r m2 m3 = 
    (case DM.lookup x m3 of
         Just c -> c
         Nothing -> 0) + (countTriplets' xs r  m2' m3')
    where m2' = DM.insertWith (+) (x*r) 1 m2
          m3' = case DM.lookup x m2 of
                    Just c' -> DM.insertWith (+) (x*r) c' m3
                    _ -> m3

lstrip = DT.unpack . DT.stripStart . DT.pack
rstrip = DT.unpack . DT.stripEnd . DT.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nrTemp <- getLine
    let nr = DL.words $ rstrip nrTemp

    let n = read (nr !! 0) :: Int

    let r = read (nr !! 1) :: Integer

    arrTemp <- getLine

    let arr = DL.map (read :: String -> Integer) . DL.words $ rstrip arrTemp

    let ans = countTriplets arr r

    -- hPutStrLn fptr $ show ans
    putStrLn $ show ans

    hFlush fptr
    hClose fptr

