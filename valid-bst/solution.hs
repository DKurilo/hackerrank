{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

data Answer = YES | NO
    deriving (Show, Eq)

check ∷ [Int] → Answer
check [] = YES
check (n:ns)
    | not err ∧ check sns ≡ YES ∧ check bns ≡ YES = YES
    | otherwise = NO
    where (sns, bns, err) = splitOn n ns

splitOn ∷ Int → [Int] → ([Int], [Int], Bool)
splitOn n ns = (\(sns, bns, err) → (reverse sns, reverse bns, err)) ∘
               foldl (\(sns, bns, err) x → 
                       if x < n ∧ bns ≡ []
                       then (x:sns, bns, err)
                       else if x > n
                       then (sns, x:bns, err)
                       else (sns,bns,True)) ([], [], False) $ ns

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    q ← getInt <$> BSC.getLine
    forM_ [1..q] $ \_ → do
        BSC.getLine
        ns ← getInts <$> BSC.getLine
        BSC.putStrLn ∘ BSC.pack ∘ show ∘ check $ ns

