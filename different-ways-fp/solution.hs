{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.List (foldl')
import Debug.Trace
import System.Environment
import System.IO

modulo ∷ Integer
modulo = 10^8 + 7

combinations ∷ Int → Int → Integer
combinations n k
   | k ≡ 0 = 1
   | k > n = 1
   | k ≤ n = foldl' (*) 1 [2..n'] `div`
             foldl' (*) 1 [2..k'] `div`
             foldl' (*) 1 [2..(n' - k')]
   where n' = fromIntegral n ∷ Integer
         k' = fromIntegral k ∷ Integer 

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    t ← getInt <$> BSC.getLine

    forM_ [1..t] $ \_ → 
        (BSC.pack ∘ show ∘ (`mod` modulo) ∘ (\(n:k:_) -> combinations n k) ∘
        getInts <$> BSC.getLine) ≫= BSC.putStrLn

