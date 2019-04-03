{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

pent ∷ Int → Int
pent 1 = 1
pent n = (3 * n - 1) * n `div` 2

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    n ← getInt <$> BSC.getLine
    forM_ [1..n] $ \_ → do
        l ← BSC.getLine
        BSC.putStrLn ∘ BSC.pack ∘ show ∘ pent ∘ getInt $ l

