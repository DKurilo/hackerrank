{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode -- so no we can use ←, ∷, etc
import Control.Monad.Unicode -- so now we can use ≫, ≫=, etc.
import Control.Monad
import Debug.Trace
import System.Environment
import System.IO
import Data.List (sortOn)
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B

data Suffices = S B.ByteString (V.Vector Int) deriving (Eq, Show)

sort ∷ B.ByteString → [Int] → [Int]
sort bs = sortOn (`B.drop` bs)

buildSA ∷ B.ByteString → Suffices
buildSA bs = S bs ∘ V.fromList ∘ sort bs $ [0..(B.length bs - 1)]

-- http://web.cs.iastate.edu/~cs548/references/linear_lcp.pdf
buildLCP ∷ Suffices → V.Vector Int
buildLCP (S bs pos) = V.replicate l 0 V.// fst hs
    where l = V.length pos
          rank = V.replicate l 0 V.// [(pos V.! i, i) | i ← [0..(l - 1)]]
          hs = foldl (\(hs', h) i → if rank V.! i > 0
                                      then let h' = go h i (pos V.! ((rank V.! i) - 1)) in
                                           ((rank V.! i, h'):hs', if h' > 0 then h' - 1 else h')
                                      else (hs', h)) ([], 0) [0..(l - 1)]
          go h i j
              | i + h ≥ l ∨ j + h ≥ l = h
              | bs `B.index` (i + h) ≡ bs `B.index` (j + h) = go (h + 1) i j
              | otherwise = h

getLargest ∷ V.Vector Int → Int
getLargest xs = flush (go [] 0 0)
    where l = V.length xs - 1
          go ∷ [Int] → Int → Int → ([Int], Int)
          go is i x
              | i > l = (is, x)
              | null is = go [i] (i + 1) x
              | xs V.! i ≥ xs V.! head is = go (i:is) (i + 1) x
              | otherwise = go (tail is) i (max x ((xs V.! head is) * (i - is !! 1)))
          flush ∷ ([Int], Int) → Int
          flush ([], x) = x
          flush (i:is, x) = flush (is, max x ((xs V.! i) * w))
              where w = if null is then i + 1 else l - head is + 1

solve ∷ B.ByteString → Int
solve bs = max (B.length bs) ((getLargest ∘ buildLCP ∘ buildSA) bs)

main ∷ IO()
main = do
    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    bs ← B.getLine

    B.putStrLn ∘ B.pack ∘ show ∘ solve $ bs
    -- B.hPutStrLn fptr ∘ B.pack ∘ show ∘ solve $ bs

    hFlush fptr
    hClose fptr
