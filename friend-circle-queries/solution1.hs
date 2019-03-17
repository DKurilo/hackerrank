{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString.Char8 as BSC
import qualified Data.IntMap as DM
import qualified Data.IntSet as DS
import Data.Array.ST
import Debug.Trace
import System.Environment
import System.IO

-- Complete the maxCircle function below.
maxCircle ∷ [[Int]] → [Int]
maxCircle queries = runST $ do
    gs ← newArray (0,100000) DS.empty ∷ ST s (STArray s Int DS.IntSet)
    gvs ← newArray (0,100000) 0 ∷ ST s (STArray s Int Int)
    (ms,_,_) ← foldM (\((m:ms),pm,gi) (p1:p2:_) → do
        case DM.lookup p1 pm of
            Just gi1 → do
                case DM.lookup p2 pm of
                    Just gi2 → do
                        vis1 ← readArray gs gi1
                        vis2 ← readArray gs gi2
                        if vis1 ≡ vis2 
                        then return (m:m:ms, pm, gi)
                        else do
                            s1 ← readArray gvs $ DS.findMin vis1
                            s2 ← readArray gvs $ DS.findMin vis2
                            let s' = s1 + s2
                            let vis = DS.union vis1 vis2
                            forM_ (DS.elems vis) $ \vi → do
                                writeArray gvs vi s'
                                writeArray gs vi vis
                            return ((max s' m):m:ms, pm, gi)
                    _ → do
                        vis ← readArray gs gi1
                        s ← readArray gvs $ DS.findMin vis
                        let s' = s + 1
                        forM_ (DS.elems vis) $ \vi → writeArray gvs vi s'
                        return ((max s' m):m:ms, DM.insert p2 gi1 pm, gi)
            _ → do
                case DM.lookup p2 pm of
                    Just gi2 → do
                        vis ← readArray gs gi2
                        s ← readArray gvs $ DS.findMin vis
                        let s' = s + 1
                        forM_ (DS.elems vis) $ \vi → writeArray gvs vi s'
                        return ((max s' m):m:ms, DM.insert p1 gi2 pm, gi)
                    _ → do
                        writeArray gs gi $ DS.singleton gi
                        writeArray gvs gi 2
                        return ((max 2 m):m:ms,
                                DM.insert p1 gi $ DM.insert p2 gi pm,
                                gi+1)) ([0],DM.empty,0) queries
    return ∘ tail ∘ reverse $ ms

readMultipleLinesAsStringArray ∷ Int → IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line ← getLine
    rest ← readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    q ← getInt <$> BSC.getLine

    queries ← forM [1..q] $ \_ → getInts <$> BSC.getLine

    let ans = maxCircle queries

    BSC.putStrLn $ BSC.intercalate "\n" $ map (\x → BSC.pack ∘ show $ x) $ ans
    -- BSC.hPutStrLn fptr $ BSC.intercalate "\n" $ map (\x → BSC.pack ∘ show $ x) $ ans

    hFlush fptr
    hClose fptr

