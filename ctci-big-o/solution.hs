{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the primality function below.
data Answer = YES | NO
instance Show Answer where
    show YES = "Prime"
    show NO = "Not prime"

primality ∷ Int → Answer
primality 1 = NO
primality 2 = YES
primality n
    | (or ∘ map (\p → n `mod` p ≡ 0) ∘ takeWhile (((≥)∘floor∘sqrt∘fromIntegral) n) $ primes) = NO
    | otherwise = YES

primes ∷ [Int]
primes = sieve [2..]

sieve ∷ [Int] → [Int]
sieve (p:xs) = p:sieve [x | x ← xs, x `mod` p > 0]

main ∷ IO()
main = do
    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    p ← readLn ∷ IO Int

    forM_ [1..p] $ \_ → do
        n <- readLn ∷ IO Int

        let result = primality n

        putStrLn ∘ show $ result
        -- hPutStrLn fptr ∘ show $ result

    hFlush fptr
    hClose fptr

