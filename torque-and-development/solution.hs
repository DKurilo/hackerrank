{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.List as DL
import qualified Data.ByteString.Char8 as BSC
import Data.List.Split
import qualified Data.Set as DS
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the roadsAndLibraries function below.
-- to compile with profiler:
-- ghc -prof -fprof-auto -rtsopts ./solution.hs
-- to run with profiler:
-- ./solution +RTS -p < in3
-- profile in solution.prof

roadsAndLibraries ∷ Int → Int → Int → [[Int]] → Int
roadsAndLibraries n c_lib c_road roads =
    c_lib * (n - (DL.foldl' (+) 0 ∘ map DS.size $ ds)) + (DL.foldl' (+) 0 ∘ map decide $ ds)
    where ds = domains roads' $ []
          roads' = DL.sort ∘ map DL.sort $ roads
          decide ∷ DS.Set Int → Int
          decide s = min (c_lib + (DS.size s - 1) * c_road) (DS.size s * c_lib)

domains ∷ [[Int]] → [DS.Set Int] → [DS.Set Int]
domains [] ds = ds
domains ((c1:c2:_):rs) ds = domains rs (DS.union d1 d2:ds'')
    where (d1,ds') = fr c1 ds
          (d2,ds'') = if DS.member c2 d1 then (DS.empty,ds') else fr c2 ds'

fr ∷ (Ord a) ⇒ a → [DS.Set a] → (DS.Set a, [DS.Set a])
fr v [] = (DS.singleton v, [])
fr v (s:ss)
    | DS.member v s = (s, ss)
    | otherwise = (s', s:ss')
    where (s', ss') = fr v ss


readMultipleLinesAsStringArray ∷ Int → IO [BSC.ByteString]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line ← BSC.getLine
    rest ← readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main ∷ IO()
main = do
    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    q ← readLn ∷ IO Int

    forM_ [1..q] $ \q_itr → do
        nmC_libC_roadTemp ← getLine
        let nmC_libC_road = words nmC_libC_roadTemp

        let n = read (nmC_libC_road !! 0) ∷ Int

        let m = read (nmC_libC_road !! 1) ∷ Int

        let c_lib = read (nmC_libC_road !! 2) ∷ Int

        let c_road = read (nmC_libC_road !! 3) ∷ Int

        citiesTemp ← readMultipleLinesAsStringArray m
        let roads = DL.map (\x → DL.map (\w → case BSC.readInt w of
                                            Just (v,_) → v
                                            _ → 0) ∘ BSC.split ' ' $ x) citiesTemp

        let result = roadsAndLibraries n c_lib c_road roads

        -- hPutStrLn fptr $ show result
        putStrLn $ show result

    hFlush fptr
    hClose fptr

