{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

kmp ∷ String → String → Bool
kmp ps cs = kmp' ps t [] cs ps t
    where t = table ps ps ps (-1)

kmp' ∷ String → [Int] → String → String → String → [Int] → Bool
kmp' _ _ _ _ [] _ = True
kmp' _ _ _ [] _ _ = False
kmp' wps wt pcs (c:cs) (p:ps) (t:ts)
      | c ≡ p = kmp' wps wt (c:pcs) cs ps ts
      | c ≠ p = kmp' wps wt [] ((reverse ∘ take t $ pcs) ⧺ [c | t ≥ 0] ⧺ cs) wps wt

table ∷ String → String → String → Int → [Int]
table [c] [] [] (-1) = [(-1)]
table _ [] _ _ = []
table (wp:wps) (cp:cps) (p:ps) i
    | cp ≡ p ∧ p ≠ wp = 0:table (wp:wps) cps ps (i + 1)
    | cp ≠ p ∧ p ≠ wp = (i+1):table (wp:wps) cps (wp:wps) (-1)
    | p ≡ wp = (-1):table (wp:wps) cps wps 0

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    n ← getInt <$> BSC.getLine
    forM_ [1..n] $ \_ → do
        cs ← BSC.unpack <$> BSC.getLine
        ps ← BSC.unpack <$> BSC.getLine
        BSC.putStrLn $ if kmp ps cs then "YES" else "NO"

