{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe
import Data.List (sort)
import Data.Map (Map, fromList, lookupGE)
import Debug.Trace
import System.IO

buildM ∷ [Int] → Map Int Int
buildM as = fromList $ zip (scanl (+) 0 (reverse ∘ sort $ as)) [0..]

find ∷ Map Int Int → Int → Int
find sm n = maybe (-1) snd (lookupGE n sm)

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    n ← getInt <$> BSC.getLine
    as ← getInts <$> BSC.getLine

    let sm = buildM as

    t ← getInt <$> BSC.getLine

    forM_ [1..t] $ \_ → BSC.pack ∘ show ∘ find sm ∘ getInt <$> BSC.getLine ≫= BSC.putStrLn

