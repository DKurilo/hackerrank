{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString.Char8 as BSC
import System.Environment
import Data.Array.ST
import Data.List as DL
import qualified Data.Array as DA
import qualified Data.IntMap as DM
import System.IO
import Debug.Trace

-- Complete the balancedForest function below.
data Node = Empty
          | Root [Int] Int Int Bool
          | Tip Int Int Bool
          | Node Int [Int] Int Int Bool
-- to value sum removed | from value removed | from to value sum removed
    deriving (Eq, Show)

type Tree s = STArray s Int Node

balancedForest ∷ Int → [Int] → [[Int]] → Int
balancedForest 0  _ _ = -1
balancedForest 1  _ _ = -1
balancedForest 2  _ _ = -1
balancedForest n c edges = runST $ do
    let vs = DA.listArray (1,n) c
    let es = DM.fromListWith (++) $ join ∘ map (\(x:y:_) → [(x,[y]),(y,[x])]) $ edges
    let bnds = (1,n)
    let rs = take 10 ∘ findRoots $ es
    let loop [] = return []
        loop (r:rs) = do
            let tos = es DM.! r
            let v = vs DA.! r
            t ← newArray bnds Empty ∷ ST s (Tree s)
            writeArray t r (Root tos v v False)
            buildT vs es t r tos
            cs' ← availableCutsWs t r tos
            if cs' ≡ []
            then loop rs
            else return cs'
    cs ← loop rs
    if cs ≡ []
    then return (-1)
    else return $ minimum cs

buildT ∷ DA.Array Int Int → DM.IntMap [Int] → Tree s → Int → [Int] → ST s ()
buildT vs es t f tos = do
    ftos ← forM tos $ \to → do
        let tos' = filter (≠f) $ es DM.! to
        let v = vs DA.! to
        if tos' ≠ []
        then writeArray t to $ Node f tos' v v False
        else writeArray t to $ Tip f v False
        updateSums t f v
        return $ (to,tos')
    forM_ ftos $ \(f',tos') → if tos' ≠ []
                              then buildT vs es t f' tos'
                              else return ()

updateSums ∷ Tree s → Int → Int → ST s ()
updateSums t k sa = do
    n ← readArray t k
    case n of
        Root ts v se False → writeArray t k (Root ts v (se + sa) False)
        Node f ts v se False → do
            writeArray t k (Node f ts v (se+sa) False)
            updateSums t f sa

findRoots ∷ DM.IntMap [Int] → [Int]
findRoots es = map snd ∘ reverse ∘ DL.sort ∘ map (\(k,tos) → (length tos,k)) ∘ DM.assocs $ es

cutMinMax ∷ Tree s → Int → ST s (Int, Int)
cutMinMax t i = do
    n ← readArray t i
    case n of
        Root _ _ ss False → return (ss `div` 3, (ss + 1)  `div` 2)
        Node _ _ _ ss False → return (ss `div` 3, (ss + 1)  `div` 2)
        _ → fail "Invalid Node"

availableCutsWs ∷ Tree s → Int → [Int] → ST s [Int]
availableCutsWs t r cns = do
    (cmin, cmax) ← cutMinMax t r
    cs ← mapM (tryCut t r cmin cmax) cns
    return $ join cs

tryCut ∷ Tree s → Int → Int → Int → Int → ST s [Int]
tryCut t r cmin cmax c = do
    cn ← readArray t c
    (exists, sa, ts) ← case cn of
        Node f ts' v sa' False | sa' ≥ cmin ∧ sa' ≤ cmax → do
            makeCut t c
            (Root _ _ sm _) ← readArray t r
            test ← findSum t sm sa' r
            unmakeCut t c
            return (test, sa', ts')
                               | sa' ≥ cmin → return (False, 0, ts')
        Tip f v False | v ≥ cmin ∧ v ≤ cmax → do
            makeCut t c
            (Root _ _ sm _) ← readArray t r
            test ← findSum t sm v r
            unmakeCut t c
            return (test, v, [])
        _ → return (False, 0, [])
    if exists
    then do
        (Root _ _ sm _) ← readArray t r
        let w = 3 * sa - sm
        cs ← availableCutsWs t r ts
        if (w ≥ 0)
        then return (w: cs)
        else return cs
    else availableCutsWs t r ts

makeCut ∷ Tree s → Int → ST s ()
makeCut t c = do
    n ← readArray t c
    case n of
        Node f ts v sa False → do
            updateSums t f (-sa)
            writeArray t c (Node f ts v sa True)
        Tip f v False → do
            updateSums t f (-v)
            writeArray t c (Tip f v True)
        _ → return ()

unmakeCut ∷ Tree s → Int → ST s ()
unmakeCut t c = do
    n ← readArray t c
    case n of
        Node f ts v sa True → do
            writeArray t c (Node f ts v sa False)
            updateSums t f sa
        Tip f v True → do
            writeArray t c (Tip f v False)
            updateSums t f v
        _ → return ()

findSum ∷ Tree s → Int → Int → Int → ST s Bool
findSum t sm sa c = do
    n ← readArray t c
    case n of
        Root ts v ss False | ss ≡ sa ∨ ss ≡ (sm - sa) → return True
                           | otherwise → or <$> mapM (findSum t sm sa) ts
        Node f ts v ss False | ss ≡ sa ∨ ss ≡ (sm - sa) → return True
                             | otherwise → or <$> mapM (findSum t sm sa) ts
        Tip f v False | v ≡ sa ∨ v ≡ (sm - sa) → return True
        _ → return False

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

    forM_ [1..q] $ \_ → do
        n ← getInt <$> BSC.getLine
        c ← getInts <$> BSC.getLine
        edges ← mapM (\_ → getInts <$> BSC.getLine) [1..n-1]

        let result = balancedForest n c edges

        BSC.putStrLn ∘ BSC.pack ∘ show $ result
        -- BSC.hPutStrLn fptr ∘ BSC.pack ∘ show $ result

    hFlush fptr
    hClose fptr

