{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

data Fate = CENTRAL | RIGHT | LEFT | DEAD
    deriving (Show)

combine ∷ [Int] → Int
combine = foldl (\n d → n * 10 + d) 0

prime ∷ Int → Bool
prime 1 = False
prime 2 = True
prime n = 
    and ∘ map (\p → n `mod` p ≠ 0) ∘ takeWhile (((≥)∘floor∘sqrt∘fromIntegral) n) $ primes

primes ∷ [Int]
primes = sieve [2..]

sieve ∷ [Int] → [Int]
sieve (p:xs) = p:sieve [x | x ← xs, x `mod` p > 0]

findFate ∷ [Int] → Fate
findFate ns
    | right ∧ left = CENTRAL
    | right = RIGHT
    | left = LEFT
    | otherwise = DEAD
    where right = check id ns
          left = check reverse ns
          check prep ns =
              foldl (\p i → let ns' = prep ∘ take i ∘ prep $ ns
                                d = head ns'
                                n = combine ns' in
                            p ∧ d ≠ 0 ∧ prime n)
                    True [1..length ns]

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    t ← getInt <$> BSC.getLine

    forM_ [1..t] $ \_ → BSC.pack ∘ show ∘ findFate ∘ map getInt ∘ BSC.groupBy (\a b → False)
                            <$> BSC.getLine ≫= BSC.putStrLn

