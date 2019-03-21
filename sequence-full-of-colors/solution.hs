{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

test ∷ String → Bool
test cs = go cs 0 0 0 0
    where go ∷ String → Int → Int → Int → Int → Bool
          go [] r g y b = r ≡ g ∧ b ≡ y
          go (c':cs') r g y b
              | abs (r - g) > 1 = False
              | abs (b - y) > 1 = False
              | otherwise = case c' of
                                'R' → go cs' (r + 1) g y b
                                'G' → go cs' r (g + 1) y b
                                'Y' → go cs' r g (y + 1) b
                                'B' → go cs' r g y (b + 1)

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    n ← getInt <$> BSC.getLine
    forM_ [1..n] $ \_ → do
        cs ← BSC.getLine
        BSC.putStrLn ∘ BSC.pack ∘ show ∘ test ∘ BSC.unpack $ cs

