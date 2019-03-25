{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

naive ∷ String → String → Bool
naive ps cs = naive' ps cs ps True

naive' ∷ String → String → String → Bool → Bool
naive' _ _ [] _ = True
naive' _ [] _ _ = False
naive' wps (c:cs) (p:ps) s
    | s ∧ c ≡ p = naive' wps cs ps False
    | not s ∧ c ≡ p = naive' wps cs ps False ∨ 
                         (if head wps ≡ c then naive' wps cs (tail wps) False else False)
    | s ∧ c ≠ p = naive' wps cs wps True
    | not s ∧ c ≠ p = naive' wps (c:cs) wps True

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    n ← getInt <$> BSC.getLine
    forM_ [1..n] $ \_ → do
        cs ← BSC.unpack <$> BSC.getLine
        ps ← BSC.unpack <$> BSC.getLine
        BSC.putStrLn $ if naive ps cs then "YES" else "NO"

