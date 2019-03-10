{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString.Char8 as BSC
import Data.Array.ST
import Data.Array
import System.Environment
import System.IO
import Debug.Trace

-- Complete the minTime function below.

data City = C Bool [(Int,Int)]
    deriving (Show, Eq)

addCity ∷ City → City → City
addCity (C m1 cs1) (C m2 cs2) = C (m1 ∨ m2) (cs1⧺cs2)

minTime ∷ Int → Int → [[Int]] → [Int] → Int
minTime n k roads machines = runST $ do
    rs ← buildG n k roads machines
    minTime' rs machines

minTime' ∷ STArray s Int City → [Int] → ST s Int
minTime' _ [] = return 0
minTime' rs (m:ms) = do
    (C ism _) ← readArray rs m
    if ism
    then do
        cost ← eliminate m rs
        cost' ← minTime' rs (m:ms)
        return $ cost + cost'
    else minTime' rs ms

buildG ∷ Int → Int → [[Int]] → [Int] → ST s (STArray s Int City)
buildG n k rs ms = do
    let bnds = (0,n-1)
    g ← newArray bnds $ C False []
    forM_ rs $ \(c1:c2:cost:_) → do
        cc1 ← readArray g c1
        cc2 ← readArray g c2
        writeArray g c1 $ addCity (C False [(c2,cost)]) cc1
        writeArray g c2 $ addCity (C False [(c1,cost)]) cc2
    forM_ ms $ \i → do
        (C _ cs) ← readArray g i
        writeArray g i (C True cs)
    return g

eliminate ∷ Int → STArray s Int City → ST s Int
eliminate m rs = do
    cut ← cutOut m rs
    case cut of
        Just (cost, c1, c2) → do
            (C ism1 cs1) ← readArray rs c1
            (C ism2 cs2) ← readArray rs c2
            writeArray rs c1 (C ism1 $ filter (\(c,co) → c≠c2) cs1)
            writeArray rs c2 (C ism2 $ filter (\(c,co) → c≠c1) cs2)
            return cost
        _ → do
            (C _ cs) ← readArray rs m
            writeArray rs m $ C False cs
            return 0

cutOut ∷ Int → STArray s Int City → ST s (Maybe (Int, Int, Int))
cutOut m rs = do
    (C _ cs) ← readArray rs m
    cutOut' m rs cs

cutOut' ∷ Int → STArray s Int City → [(Int,Int)] → ST s (Maybe (Int, Int, Int))
cutOut' _ _ [] = return Nothing
cutOut' m rs ((c,cost):cs) = do
    (C ism cs') ← readArray rs c
    if ism 
    then return $ Just (cost, m, c)
    else do
        step ← cutOut' c rs $ foldr (\(c',cost') acs → if c' ≡ m
                                                       then acs
                                                       else (c',cost'):acs) [] cs'
        case step of
            Nothing → cutOut' m rs cs
            Just (cost', _, _) → if cost < cost'
                                 then return $ Just (cost, m, c)
                                 else return step

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    stdout ← getEnv "OUTPUT_PATH"

    fptr ← openFile stdout WriteMode

    (n:k:_) ← getInts <$> BSC.getLine

    roads ← forM [1..n - 1] $ \_ → getInts <$> BSC.getLine
    machines ← mapM (\_ → getInt <$> BSC.getLine) [1..k]

    let result = minTime n k roads machines

    BSC.putStrLn ∘ BSC.pack ∘ show $ result
    -- BSC.hPutStrLn fptr ∘ BSC.pack ∘ show $ result

    hFlush fptr
    hClose fptr

