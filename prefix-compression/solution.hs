{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

-- function is here
newtype Line = L String
instance Show Line where
    show (L "") = "0"
    show (L l) = (show ∘ length $ l) ⧺ " " ⧺ l

data Answer = A Line Line Line -- prefix line1 line2
instance Show Answer where
    show (A l1 l2 l3) = BSC.unpack ∘ BSC.intercalate "\n" ∘ map (BSC.pack ∘ show) $ [l1, l2, l3]

compress ∷ String → String → Answer
compress "" cs = A (L "") (L "") (L cs)
compress cs "" = A (L "") (L cs) (L "")
compress (c1:cs1) (c2:cs2)
    | c1 ≡ c2 = A (L $ c1:p) (L cs1') (L cs2')
    | otherwise = A (L "") (L $ c1:cs1) (L $ c2:cs2)
    where A (L p) (L cs1') (L cs2') = compress cs1 cs2

main ∷ IO()
main = do
    cs1 ← BSC.unpack <$> BSC.getLine
    cs2 ← BSC.unpack <$> BSC.getLine
    let ans = compress cs1 cs2

    BSC.putStrLn ∘ BSC.pack $ show ans

