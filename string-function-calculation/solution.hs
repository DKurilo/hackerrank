{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode -- so no we can use ←, ∷, etc
import Control.Monad.Unicode -- so now we can use ≫, ≫=, etc.
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Debug.Trace
import System.Environment
import System.IO
import qualified Data.Map as DM

solve ∷ B.ByteString → Int
solve cs = max l (go 0 0 [0..(l - 1)])
    where l = B.length cs
          go _ lm [] = lm
          go i lm ps = maximum (cm:nms)
              where m = DM.fromListWith (\(p, _) (ps', pl) → (head p:ps', pl + 1)) ∘
                        map (\k → let c = B.index cs (k + i) in (c, ([k], 1))) ∘
                        filter (\k → k + i < l) $ ps
                    cm = maximum ∘ map (\(_, pl) → (i + 1) * pl) ∘ DM.elems $ m
                    nms = if cm ≥ lm
                            then  map (\(ps', _) → go (i + 1) cm ps') ∘
                                                   filter (\(_, pl) → pl > 1) ∘ DM.elems $ m
                            else []

main ∷ IO()
main = do
    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    cs ← B.getLine

    B.putStrLn ∘ B.pack ∘ show ∘ solve $ cs
    -- B.hPutStrLn fptr ∘ B.pack ∘ show ∘ solve $ cs

    hFlush fptr
    hClose fptr
