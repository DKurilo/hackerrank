{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.Environment
import System.IO

-- function is herea
type Queen = (Int, Int)

ways ∷ Int → Int
ways n = ways' n [] 0 0

ways' ∷ Int → [Queen] → Int → Int → Int
ways' n qs x y
    | x ≥ n = 0
    | y ≥ n = 1
    | otherwise = ways' n qs (x+1) y + 
                  (if (try qs x y)
                   then ways' n ((x,y):qs) 0 (y+1)
                   else 0)

try ∷ [Queen] → Int → Int → Bool
try [] _ _ = True
try qs x y = and ∘ map check $ qs
    where check ∷ Queen → Bool
          check (qx, qy)
              | qx ≡ x ∨ qy ≡ y = False
              | dx ≡ dy = False
              | dx ≤ 2 ∧ dy ≤ 2 = False
              | otherwise = True
              where dx = abs (qx - x)
                    dy = abs (qy - y)

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0
    n ← getInt <$> BSC.getLine
    let ans = ways n 
    BSC.putStrLn ∘ BSC.pack ∘ show $ ans

