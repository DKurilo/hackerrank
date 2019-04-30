{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.Set (Set, fromList, intersection, size)
import Debug.Trace
import System.IO

divisors ∷ Int → Int → Int
divisors m l = size $ intersection (divs m) (divs l)

divs ∷ Int → Set Int
divs x = fromList $ 
    x:join [[y, x `div` y] | y ← [1..(ceiling ∘ sqrt ∘ fromIntegral $ x)], x `mod` y ≡ 0]

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    t ← getInt <$> BSC.getLine

    forM_ [1..t] $ \_ → do
        (m:l:_) ← getInts <$> BSC.getLine
        BSC.putStrLn ∘ BSC.pack ∘ show $ divisors m l

