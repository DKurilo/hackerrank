{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.Array.Unboxed
import qualified Data.Map as DM
import Debug.Trace
import System.IO

type State = (Int, Int)

state ∷ Int → Int → State
state f1 f2 = (min f1 f2, max f1 f2)

time ∷ Int → [Int] → Int
time n as = fst $ time' las n DM.empty 0 (-1, -1)
    where las = (listArray (0, n - 1) $ map (\a → if a ≡ 0 then 10 else a) as)

time' ∷ UArray Int Int → Int → DM.Map State Int → Int → State → (Int, DM.Map State Int)
time' as n tm k (f1, f2)
    | n ≡ k = (0, tm)
    | otherwise = case DM.lookup (f1, f2) tm of
        Just t → (t, tm)
        _ | f1 ≥ 0 ∧ as ! f1 ≡ as ! k → (t1, DM.insert (f1, f2) t1 tm1)
          | f2 ≥ 0 ∧ as ! f2 ≡ as ! k → (t2, DM.insert (f1, f2) t2 tm2)
          | otherwise  →  (mt, DM.insert (f1, f2) mt tm12)
    where s1 = state k f2
          s2 = state f1 k
          (t1', tm1) = time' as n tm (k + 1) s1
          t1 = t1' + 1 + if f1 ≡ -1 then 0 else abs $ as ! f1 - as ! k
          (t12', tm12) = time' as n tm1 (k + 1) s2
          t12 = t12' + 1 + if f2 ≡ -1 then 0 else abs $ as ! f2 - as ! k
          (t2', tm2) = time' as n tm (k + 1) s2
          t2 = t2' + 1 + (abs $ as ! f2 - as ! k)
          mt = min t1 t12

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0
    let getInts = map getInt <$> BSC.split ' '
    n ← getInt <$> BSC.getLine
    BSC.getLine ≫= BSC.putStrLn ∘ BSC.pack ∘ show ∘ time n ∘ getInts

