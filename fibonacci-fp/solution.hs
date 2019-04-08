{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

modulo ∷ Integer
modulo = 10^8 + 7

fib ∷ Int → Integer
fib = (map fib' [0..] !!)
    where fib' 0 = 0
          fib' 1 = 1
          fib' n = fib (n - 1) + fib (n - 2)

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0
    t ← getInt <$> BSC.getLine
    forM_ [1..t] $ \_ → BSC.pack ∘ show ∘ (`mod` modulo) ∘ fib ∘ getInt <$>
                        BSC.getLine ≫= BSC.putStrLn

