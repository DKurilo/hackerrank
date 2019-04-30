{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as DM
import Debug.Trace
import System.IO

difference ∷ [Int] → [Int] → [Int]
difference ns ms = DM.keys ∘ DM.filter (≠0) $ DM.unionWith (-) (m ms) (m ns)
    where m as = DM.fromListWith (+) $ zip as $ repeat 1

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    n ← getInt <$> BSC.getLine
    ns ← getInts <$> BSC.getLine

    m ← getInt <$> BSC.getLine
    ms ← getInts <$> BSC.getLine

    BSC.putStrLn ∘ BSC.intercalate " " ∘ map (BSC.pack ∘ show) $ difference ns ms

