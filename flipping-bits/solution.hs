{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE  DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Data.Array
import Data.Bits
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the flippingBits function below.
flippingBits ∷ Int → Int
flippingBits n = dec ∘ invert ∘ bin 32 $ n

bin ∷ Int -> Int → String
bin 0 _ = []
bin l 0 = take l ∘ repeat $ '0'
bin l 1 = (take (l - 1) ∘ repeat $ '0') ⧺ "1"
bin l n = bin (l-1) d ⧺ bin 1 m
    where (d,m) = divMod n 2

invert ∷ String → String
invert [] = []
invert (c:cs)
    | c == '0' = '1': invert cs
    | otherwise = '0': invert cs

dec ∷ String → Int
dec n = dec' ∘ reverse $ n

dec' ∷ String → Int
dec' [] = 0
dec' (c:cs)
    | c == '0' = 2 * dec' cs
    | otherwise = 1 + 2 * dec' cs

main ∷ IO()
main = do
    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    q ← readLn ∷ IO Int

    forM_ [1..q] $ \q_itr → do
        n ← readLn ∷ IO Int

        let result = flippingBits n

        -- hPutStrLn fptr $ show result
        putStrLn $ show result

    hFlush fptr
    hClose fptr

