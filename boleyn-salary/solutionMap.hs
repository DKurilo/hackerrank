{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString.Char8 as BSC
import Data.List (sort)
import Data.Array
import Data.Array.ST
import qualified Data.Map as DM
import Debug.Trace
import System.IO

data Emp = Emp Int Int (DM.Map Int Int) | Top Int (DM.Map Int Int)
    deriving (Show)

buildA ∷ [(Int, Int)] → Array Int Int → Array Int Emp
buildA rs ss = runSTArray $ do
    let (i0, ie) = bounds ss
    arr ← newArray_ (i0, ie) ∷ ST s (STArray s Int Emp)
    forM_ [i0..ie] $ \i → writeArray arr i $ Top (ss ! i) DM.empty
    forM_ rs $ \(ei, bi) → do
        (Top s ar) ← readArray arr ei
        let emp' = Emp bi s ar
        writeArray arr ei emp'
        updateBosses arr bi $ DM.insert s ei ar
    return arr

updateBosses ∷ STArray s Int Emp → Int → DM.Map Int Int → ST s ()
updateBosses arr bi sar = do
        boss ← readArray arr bi
        case boss of
            Emp bi' s ar → do
                let ar' = DM.union ar sar
                let emp = Emp bi' s ar'
                writeArray arr bi emp
                updateBosses arr bi' ar'
            Top s ar → writeArray arr bi $ Top s $ DM.union ar sar

employee ∷ Array Int Emp → Int → Int → Int
employee es i k = case es ! i of
    Emp _ _ ss | ss ≡ DM.empty → k
               | otherwise → snd $ DM.elemAt (k - 1) ss
    Top _ ss → snd $ DM.elemAt (k - 1) ss

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    (n:q:_) ← getInts <$> BSC.getLine
    
    rs ← reverse ∘ sort <$> (forM [1..n - 1] $ \_ → (\(x:y:_) → (x, y)) ∘ getInts <$> BSC.getLine)
    ss ← listArray (1, n) ∘ getInts <$> BSC.getLine
    let es = buildA rs ss

    foldM_ (\v _ → do
        v' ← (\(d:k:_) → employee es (v + d) k) ∘ getInts <$> BSC.getLine
        BSC.putStrLn ∘ BSC.pack ∘ show $ v'
        return v') 0 [1..q]

