{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import qualified Data.ByteString.Char8 as BSC
import Data.Graph
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
    DL.foldl' (\s c → s + (decide ∘ length $ c)) 0 $! dff $! 
    buildG (1,n) ∘ foldr (\(c1:c2:_) rs → ((c1,c2):(c2,c1):rs)) [] $! roads
    where decide ∷ Int → Int
          decide s = min (c_lib + (s - 1) * c_road) (s * c_lib)

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

