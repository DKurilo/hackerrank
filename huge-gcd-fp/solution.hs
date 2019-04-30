{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

modulo ∷ Integer
modulo = 10^9 + 7

gcdL ∷ [Integer] → [Integer] → Integer
gcdL ns ms = gcd (product ns) (product ms)

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → fromIntegral x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    n ← getInt <$> BSC.getLine
    ns ← getInts <$> BSC.getLine

    m ← getInt <$> BSC.getLine
    ms ← getInts <$> BSC.getLine

    BSC.putStrLn ∘ BSC.pack ∘ show ∘ (`mod` modulo) $ gcdL ns ms

