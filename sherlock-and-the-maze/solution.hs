{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import Data.List (foldl')
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

modulo ∷ Integer
modulo = 10^9 + 7

parts ∷ Int → Int → Integer
parts k n = memparts !! n !! k

memparts ∷ [[Integer]]
memparts = [[go k n | k ← [0..]] | n ← [0..]]
    where go 0 _ = 0
          go 1 1 = 1
          go k 1 = 0
          go 1 n = 1
          go k n = foldl' (\a x → a + parts (k - 1) (n - x)) 0 [1..(n - k + 1)]

ways ∷ Int → Int → Int → Integer
ways 1 1 0 = 1 
ways n m 0 = 0 
ways 1 _ _ = 1
ways _ 1 _ = 1
ways n m k = parts k1 n' * parts k2 m' + parts k1 m' * parts k2 n' + ways n m (k - 1)
    where n' = n - 1
          m' = m - 1
          k1 = (k + 1) `div` 2
          k2 = k + 1 - k1

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    t ← getInt <$> BSC.getLine

    forM_ [1..t] $ \_ → BSC.getLine ≫= BSC.putStrLn ∘ BSC.pack ∘ show ∘ 
                        (`mod` modulo) ∘ (\(n:m:k:_) → ways n m k) ∘ getInts

