{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

gcd2 ∷ Int → Int → Int
gcd2 x 0 = x
gcd2 0 x = x
gcd2 x y
    | x > y = gcd2 y $ x `mod` y
    | otherwise = gcd2 x $ y `mod` x

lcd2 :: Int → Int → Int
lcd2 x y
    | d ≡ 1 = x * y
    | otherwise = d * lcd2 (x `div` d) (y `div` d)
    where d = gcd2 x y

lcd ∷ [Int] → Int
lcd [] = 0 -- error
lcd [x] = x
lcd (x1:x2:xs) = lcd $ lcd2 x1 x2:xs

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    n ← getInt <$> BSC.getLine
    js ← getInts <$> BSC.getLine

    BSC.putStrLn ∘ BSC.pack ∘ show ∘ lcd $ js

