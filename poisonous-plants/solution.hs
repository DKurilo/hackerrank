{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
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

-- Complete the poisonousPlants function below.
poisonousPlants ∷ [Int] → Int
poisonousPlants [] = 0
poisonousPlants ps
    | r ≡ 0 = 0
    | otherwise = 1 + poisonousPlants (p1:ps')
    where (r, ps') = day p1 ∘ tail $ ps
          p1 = head ps

day ∷ Int → [Int] → (Int, [Int])
day p1 [] = (0, [])
day p1 (p:ps)
    | p1 < p = (r+1, ps')
    | otherwise = (r, p:ps')
    where (r, ps') = day p ps

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

    pTemp <- getLine

    let p = Data.List.map (read :: String -> Int) . words $ pTemp

    let result = poisonousPlants p

    putStrLn $ show result
    -- hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr

