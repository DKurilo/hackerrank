{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

sums ∷ Int → Int → Int
sums x n = sums' x n 1 m
    where m = floor $ (fromIntegral x) ** (1 / (fromIntegral n))

sums' ∷ Int → Int → Int → Int → Int
sums' x n k m
    | x < 0 = 0
    | x ≡ 0 = 1
    | k > m = 0
    | otherwise = sum ∘ map (\y → sums' (x - y ^ n) n (y + 1) m) $ [k..m]

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    x ← getInt <$> BSC.getLine
    n ← getInt <$> BSC.getLine
    
    let ans = sums x n

    BSC.putStrLn ∘ BSC.pack ∘ show $ ans

