{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import System.IO
import Data.Maybe
import Data.List.Split
import Control.Monad
import System.IO

import Debug.Trace

class Queue a where
    put ∷ a b → b → a b
    pop ∷ a b → a b
    peek ∷ a b → Maybe b

data FIFO a = FIFOEmpty | FIFO [a] [a]
    deriving (Eq, Show)
instance Queue FIFO where
    put FIFOEmpty a = FIFO [] [a]
    put (FIFO as []) a = FIFO [a] $ reverse as
    put (FIFO as ras) a = FIFO (a:as) ras
    pop FIFOEmpty = FIFOEmpty
    pop (FIFO (_:[]) []) = FIFOEmpty
    pop (FIFO as []) = FIFO [] $ tail ∘ reverse $ as
    pop (FIFO [] (_:[])) = FIFOEmpty
    pop (FIFO as (ra:[])) = FIFO [] $ reverse as
    pop (FIFO as ras) = FIFO as $ tail ras
    peek FIFOEmpty = Nothing
    peek (FIFO _ (ra:_)) = Just ra

data LIFO a = LIFOEmpty | LIFO [a]
    deriving (Eq, Show)
instance Queue LIFO where
    put LIFOEmpty a = LIFO [a]
    put (LIFO as) a = LIFO (a:as)
    pop LIFOEmpty = LIFOEmpty
    pop (LIFO (_:[])) = LIFOEmpty
    pop (LIFO (a:as)) = LIFO as
    peek LIFOEmpty = Nothing
    peek (LIFO (a:as)) = Just a

process ∷ (Queue q) ⇒ q Int → [[Int]] → [Int]
process q [] = []
process q (c:cs)
    | c ≡ [] = process q cs
    | cmd ≡ 1 = process (put q (c !! 1)) cs
    | cmd ≡ 2 = process (pop q) cs
    | cmd ≡ 3 = case peek q of
        Just v -> v:process q cs
        Nothing -> process q cs
    | otherwise = process q cs
    where cmd = head c

main ∷ IO()
main = do
    t <- readLn ∷ IO Int
    
    cs <- forM [1..t] $ \_ → do
        css <- getLine
        return $ map (read ∷ String → Int) ∘ splitOn " " $ css

    let result = process (FIFOEmpty :: FIFO Int) cs

    forM_ result $ \n → putStrLn ∘ show $ n

