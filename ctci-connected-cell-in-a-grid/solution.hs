{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Data.Array
import Data.Bits
import Data.List as DL
import Data.List.Split
import Data.Set as DS
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the maxRegion function below.
type Grid = Array (Int,Int) Int
maxRegion ∷ [[Int]] → Int → Int → Int
maxRegion grid w h = maximum ∘ maxRegion' $ g
    where bnds = ((-1,-1),(w,h))
          g = array bnds [((x,y),if x≡(-1) ∨ y≡(-1) ∨ x≡w ∨ y≡h then 0 else grid !! y !! x) |
                          x ← [(-1)..w], y ← [(-1)..h]]

maxRegion' ∷ Grid → [Int]
maxRegion' g = fst $ DL.foldl' go ([],empty) $ indices g
    where go ∷ ([Int], Set (Int,Int)) → (Int,Int) → ([Int], Set (Int,Int))
          go (ss, ps) i
              | g ! i ≡ 1 ∧ i `notMember` ps = (s':ss, ps')
              | otherwise = (0:ss, ps)
              where (s', ps') = dfs g (0,ps) i

dfs ∷ Grid → (Int, Set (Int,Int)) → (Int,Int) → (Int, Set (Int,Int))
dfs g (s,ps) (x,y)
    | g ! (x,y) ≡ 1 ∧ (x,y) `notMember` ps = DL.foldl' (dfs g) (s+1,DS.insert (x,y) ps) $
                                                       around (x,y)
    | otherwise = (s,ps)

around ∷ (Int,Int) → [(Int,Int)]
around (x,y) = [(x',y') | x' ← [x-1..x+1], y' ← [y-1..y+1], x' ≠ x ∨ y' ≠ y]

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

    m ← readLn ∷ IO Int

    gridTemp ← readMultipleLinesAsStringArray n
    let grid = DL.map (\x → DL.map (read ∷ String → Int) . words $ x) gridTemp

    let res = maxRegion grid m n

    -- hPutStrLn fptr $ show res
    putStrLn $ show res

    hFlush fptr
    hClose fptr

