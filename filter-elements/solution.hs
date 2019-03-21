{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.IntMap as DM
import qualified Data.List as DL
import Debug.Trace
import System.Environment
import System.IO

find ∷ Int → [Int] → [Int]
find r as = if as' ≡ [] then [-1] else as'
    where as' = map snd ∘ DL.sort ∘ map (\(k, i) → (i, k)) ∘ DM.assocs ∘ DM.map fst ∘
                DM.filter (\(_, n) → n ≥ r) ∘ DM.fromListWith (\(_, _) (i, n) → (i, n + 1)) $
                zip as [(i, 1) | i ← [1..]]

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    t ← getInt <$> BSC.getLine

    forM_ [1..t] $ \_ → do
        (n:r:_) ← getInts <$> BSC.getLine
        as ← getInts <$> BSC.getLine
        BSC.putStrLn ∘ BSC.intercalate " " ∘ map (BSC.pack ∘ show) ∘ find r $ as

