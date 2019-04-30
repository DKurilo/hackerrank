{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.List (sort)
import Debug.Trace
import System.IO

data Guest = G Int Int

kth ∷ Int → Guest → Int
kth k (G a h) = a + (k - 1) * h

tryN ∷ Int → [Guest] → Int → Bool
tryN m gs n = (≤m) ∘ sum ∘ take n ∘ sort ∘ map (kth n) $ gs

guests ∷ Int → [Guest] → Int
guests m gs = guess 1 $ length gs
    where guess ib ie
              | ie - ib ≤ 1 ∧ tryN m gs ie = ie
              | ie - ib ≤ 1 ∧ tryN m gs ib = ib
              | ie - ib ≤ 1 = ib - 1
              | tryN m gs im = guess im ie
              | otherwise = guess ib im
              where im = (ib + ie) `div` 2

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    (n:m:_) ← getInts <$> BSC.getLine
    as ← getInts <$> BSC.getLine
    hs ← getInts <$> BSC.getLine

    BSC.putStrLn ∘ BSC.pack ∘ show ∘ guests m $ zipWith (\a h → G a h) as hs

