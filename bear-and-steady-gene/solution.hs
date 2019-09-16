{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Prelude.Unicode -- so no we can use ←, ∷, etc
import Control.Monad.Unicode -- so now we can use ≫, ≫=, etc.
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.Environment
import System.IO

data Gene = C | G | A | T | Empty deriving (Eq, Show)
type Slice = (Int, Gene, Int, Int, Int, Int)
prepare ∷ BSC.ByteString → [Slice]
prepare = BSC.foldl addGene [(0, Empty, 0, 0, 0, 0)]
    where addGene gs@((p, g, gc, gg, ga, gt):_) 'C' = (p + 1, C, gc + 1, gg, ga, gt):gs
          addGene gs@((p, g, gc, gg, ga, gt):_) 'G' = (p + 1, G, gc, gg + 1, ga, gt):gs
          addGene gs@((p, g, gc, gg, ga, gt):_) 'A' = (p + 1, A, gc, gg, ga + 1, gt):gs
          addGene gs@((p, g, gc, gg, ga, gt):_) 'T' = (p + 1, T, gc, gg, ga, gt + 1):gs
          addGene gs _ = gs

findAll ∷ [Slice] → [Int]
findAll [] = [0]
findAll sls = if gc ≡ 0 ∧ gg ≡ 0 ∧ ga ≡ 0 ∧ gt ≡ 0 then [0] else go psls psls []
    where (_, _, cgc, cgg, cga, cgt) = head sls
          t = cgc + cgg + cga + cgt
          gn = t `div` 4
          (gc, gg, ga, gt) = (check cgc, check cgg, check cga, check cgt)
          check x = if x > gn then x - gn else 0
          psls = filter canBeEnd sls
          canBeEnd (_, C, _, _, _, _) = gc > 0
          canBeEnd (_, G, _, _, _, _) = gg > 0
          canBeEnd (_, A, _, _, _, _) = ga > 0
          canBeEnd (_, T, _, _, _, _) = gt > 0
          canBeEnd (_, _, _, _, _, _) = False
          go [] _ ls = ls
          go sls@((ps, gs, gcs, ggs, gas, gts):ss) slf@((pf, _, gcf, ggf, gaf, gtf):fs) ls
              | check C ∧ check G ∧ check A ∧ check T = go sls fs (dp:ls)
              | otherwise = go ss slf ls
              where dp = pf - ps + 1
                    dgc = gcf - gcs + (if gs ≡ C then 1 else 0)
                    dgg = ggf - ggs + (if gs ≡ G then 1 else 0)
                    dga = gaf - gas + (if gs ≡ A then 1 else 0)
                    dgt = gtf - gts + (if gs ≡ T then 1 else 0)
                    check C = gc ≡ 0 ∨ dgc ≥ gc
                    check G = gg ≡ 0 ∨ dgg ≥ gg
                    check A = ga ≡ 0 ∨ dga ≥ ga
                    check T = gt ≡ 0 ∨ dgt ≥ gt

main ∷ IO()
main = do
    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode
    _ ← BSC.getLine
    BSC.getLine ≫=
        (BSC.putStrLn ∘
        -- (BSC.hPutStrLn fptr ∘
            BSC.pack ∘ show ∘ minimum ∘ findAll ∘ prepare)
    hFlush fptr
    hClose fptr
