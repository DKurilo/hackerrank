{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

data Point = P Int Int
    deriving (Eq)

instance Show Point where
    show (P r c) = show r ⧺ " " ⧺ show c

(.+) ∷ Point → Point → Point
(P r1 c1) .+ (P r2 c2) = P (r1 + r2) (c1 + c2)

(.-) ∷ Point → Point → Point
(P r1 c1) .- (P r2 c2) = P (r1 - r2) (c1 - c2)

data Tromino = T Point Point
    deriving (Eq)

instance Show Tromino where
    show = unwords ∘ map show ∘ tpoints

tpoints ∷ Tromino → [Point]
tpoints (T (P r c) pe) = [P (r + r') (c + c') | r' ← [0, 1], c' ← [0, 1], P r' c' ≠ pe]

tAddP ∷ Tromino → Point → Tromino
tAddP (T pb pe) p = T (pb .+ p) (pe .+ p)

inSquare ∷ Point → Point → Point → Bool
inSquare (P r1 c1) (P r2 c2) (P r c) = r > r1 ∧ c > c1 ∧ r ≤ r2 ∧ c ≤ c2

solve ∷ Int → Point → [Tromino]
solve m = cover m (P 0 0)

cover ∷ Int → Point → Point → [Tromino]
cover 0 _ _ = []
cover 1 pa0 pe = [T (pa0 .+ P 1 1) (pe .- P 1 1)]
cover m pa0 pe = T (pa0 .+ P w2 w2) pe':
                 (join ∘ map (\(p0, pe) → cover (m - 1) (p0 .+ pa0) (pe .- p0)) $ ps)
    where w2 = 2 ^ (m - 1)
          getPe pd p0 pbr pe = if inSquare p0 pbr pe then pe else pd
          ps = [let p0 = P (w2 * r) (w2 * c)
                    pbr = P w2 w2 .+ p0 in
                (p0, getPe (P (w2 + r) (w2 + c)) p0 pbr pe) |
                 r ← [0, 1], c ← [0, 1]]
          pe' = head [P r c | r ← [0, 1], c ← [0, 1], snd (ps !! (r * 2 + c)) ≡ pe]

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    m ← getInt <$> BSC.getLine
    (r:c:_) ← getInts <$> BSC.getLine

    BSC.putStrLn ∘ BSC.intercalate "\n" ∘ map (BSC.pack ∘ show) $ solve m (P r c)

