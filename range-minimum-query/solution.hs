{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

inf :: Int
inf = 10000000

data STree = Tip Int Int | Node STree Int Int Int STree | Empty Int
    deriving (Show, Eq)

val ∷ STree → Int
val (Tip _ v) = v
val (Empty _) = inf
val (Node _ _ v _ _) = v

buildT ∷ Int → Int → [Int] → STree
buildT b _ [] = Empty b
buildT b _ [a] = Tip b a
buildT b e as = Node lt b (min (val lt) (val rt)) e rt
    where m = b + (e - b) `div` 2
          lt = buildT b m $ take (m - b + 1) as
          rt = buildT (m + 1) e $ drop (m - b + 1) as

queryT ∷ STree → (Int, Int) → Int
queryT (Empty i) (b, e) = inf
queryT (Tip i v) (b, e) = if b ≤ i ∧ e ≥ i then v else inf
queryT (Node lt ib v ie rt) (b, e)
    | ib ≥ b ∧ ie ≤ e = v
    | e < ib ∨ b > ie = inf
    | otherwise = min (queryT lt (b, e)) (queryT rt (b, e))

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    (n:m:_) ← getInts <$> BSC.getLine
    t ← buildT 0 (n - 1) ∘ getInts <$> BSC.getLine

    forM_ [1..m] $ \_ → do
        (b:e:_) ← getInts <$> BSC.getLine
        BSC.putStrLn ∘ BSC.pack ∘ show ∘ queryT t $ (b, e)

