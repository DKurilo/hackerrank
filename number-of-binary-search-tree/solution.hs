{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

prime ∷ Integer
prime = 10 ^ 8 + 7

trees ∷ Int → Integer
trees = (map tr [0..] !!)
    where tr 0 = 1
          tr 1 = 1
          tr n = sum [trees i * trees (n - 1 - i) | i ← [0..(n - 1)]]

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    n ← getInt <$> BSC.getLine
    forM_ [1..n] $ \_ → do
        l ← BSC.getLine
        BSC.putStrLn ∘ BSC.pack ∘ show ∘ (`mod` prime) ∘ trees ∘ getInt $ l

