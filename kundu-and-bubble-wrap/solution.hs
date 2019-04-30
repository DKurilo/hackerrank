{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import Data.List (foldl')
import Data.List.Split (splitOn)
import Numeric
import Debug.Trace
import System.IO

-- expectation ∷ Double → Double
-- expectation n = go (n - 1)
--     where go ∷ Double → Double
--           go 0 = 1.0
--           go k = 1.0 + foldl' (\r _ → p * s + (1 - p) * (1 + r)) 1.0 [1..20000]
--               where p = k / n
--                     s = go $ k - 1.0

-- check image here: https://github.com/DKurilo/hackerrank/kundu-and-bubble-wrap/
expectation ∷ Double → Double
expectation n = go n
    where go ∷ Double → Double
          go 0 = 0.0
          go k = 1.0 + (go $ k - 1.0) - 1.0 + n / k

main ∷ IO()
main = (\v → showFFloat Nothing v "") ∘ expectation ∘ product ∘ map (read) ∘ splitOn " " <$>
       getLine ≫= putStrLn

