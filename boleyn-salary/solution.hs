{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString.Char8 as BSC
import Data.Array
import Data.Array.ST
import Debug.Trace
import System.IO

data Emp = Emp Int Int (Array Int (Int, Int)) | Top Int (Array Int (Int, Int))
    deriving (Show)

buildA ∷ [(Int, Int)] → Array Int Int → Array Int Emp
buildA rs ss = runSTArray $ do
    let (i0, ie) = bounds ss
    arr ← newArray_ (i0, ie) ∷ ST s (STArray s Int Emp)
    forM_ [i0..ie] $ \i → writeArray arr i $ Top (ss ! i) $ array (1,0) []
    forM_ rs $ \(ei, bi) → do
        (Top s ar) ← readArray arr ei
        let emp' = Emp bi s ar
        writeArray arr ei emp'
        updateBosses arr bi $ mergeArs ar $ listArray (1,1) [(s, ei)]
    return arr

updateBosses ∷ STArray s Int Emp → Int → Array Int (Int, Int) → ST s ()
updateBosses arr bi sar = do
        boss ← readArray arr bi
        case boss of
            Emp bi' s ar → do
                let ar' = mergeArs ar sar
                let emp = Emp bi' s ar'
                writeArray arr bi emp
                updateBosses arr bi' ar'
            Top s ar → writeArray arr bi $ Top s $ mergeArs ar sar

mergeArs ∷ Array Int (Int, Int) → Array Int (Int, Int) → Array Int (Int, Int)
mergeArs a1 a2 = listArray (1, size a1 + size a2) $ mergeLists (elems a1) (elems a2)
    where size = (\(i0, i1) → i1 - i0 + 1) ∘ bounds

mergeLists ∷ [(Int, Int)] → [(Int, Int)] → [(Int, Int)]
mergeLists [] bs = bs
mergeLists as [] = as
mergeLists ((av, ak):as) ((bv, bk):bs)
    | av ≥ bv = (bv, bk):mergeLists ((av, ak):as) bs
    | otherwise = (av, ak):mergeLists as ((bv, bk):bs)

employee ∷ Array Int Emp → Int → Int → Int
employee es i k = case es ! i of
    Emp _ _ ss | size ss ≤ 0 → i
               | otherwise → snd $ ss ! k
    Top _ ss → snd $ ss ! k
    where size = (\(i0, ie) → ie - i0 + 1) ∘ bounds

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    (n:q:_) ← getInts <$> BSC.getLine
    
    rs ← reverse <$> (forM [1..n - 1] $ \_ → (\(x:y:_) → (x, y)) ∘ getInts <$> BSC.getLine)
    ss ← listArray (1, n) ∘ getInts <$> BSC.getLine
    let es = buildA rs ss

    foldM_ (\v _ → do
        v' ← (\(d:k:_) → employee es (v + d) k) ∘ getInts <$> BSC.getLine
        BSC.putStrLn ∘ BSC.pack ∘ show $ v'
        return v') 0 [1..q]

