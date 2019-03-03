{-# LANGUAGE FlexibleInstances, UndecidableInstances  #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.Array.Unboxed as AU
import Data.Bits
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the riddle function below.
riddle ∷ [Int] → [Int]
riddle arr = riddle' (AU.listArray (0,l) arr) 0 l $ DL.sortOn rev $ zip arr [0..]
    where l = length arr - 1
          rev (a,b) = (-a,b)

riddle' ∷ AU.UArray Int Int → Int → Int → [(Int,Int)] → [Int]
riddle' aa _ _ [] = [] -- something goes wrong
riddle' _ i l ((a, _):[]) = take (l - i + 1) ∘ repeat $ a
riddle' aa i l ((a, ia):as)
    | i ≡ l + 1 = []
    | m ≥ i = (take k ∘ repeat $ a) ⧺ riddle' aa (i+k) l as
    | otherwise = riddle' aa i l as
    where m = (left a ia aa) + (right a ia aa)
          k = m - i + 1

left ∷ Int → Int → AU.UArray Int Int → Int
left _ 0 _ = 0
left a i aa
    | aa AU.! (i - 1) ≥ a = 1 + (left a (i - 1) aa)
    | otherwise = 0


right ∷ Int → Int → AU.UArray Int Int → Int
right a i aa
    | i ≥ size = 0
    | aa AU.! (i + 1) ≥ a = 1 + (right a (i + 1) aa)
    | otherwise = 0
    where size = (\(a, b) → b - a) ∘ AU.bounds $ aa

-- complete this function

readMultipleLinesAsStringArray ∷ Int → IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line ← getLine
    rest ← readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main ∷ IO()
main = do
    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    n ← readLn ∷ IO Int

    arrTemp ← getLine

    let arr = DL.map (read ∷ String → Int) ∘ words $ arrTemp

    let res = riddle arr

    -- hPutStrLn fptr $ DL.intercalate " " $ DL.map (\x → show x) $ res
    putStrLn $ DL.intercalate " " $ DL.map (\x → show x) $ res

    hFlush fptr
    hClose fptr

