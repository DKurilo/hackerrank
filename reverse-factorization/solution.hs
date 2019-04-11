{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Data.Ord
import Control.Monad.Unicode
import Control.Monad
import Data.List (sort, foldl')
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

newtype Seq = S [Int]
    deriving (Show, Eq)

instance Ord Seq where
    compare (S []) (S []) = EQ
    compare (S []) (S (_:_)) = LT
    compare (S (_:_)) (S []) = GT
    compare (S (s1:ss1)) (S (s2:ss2))
        | l1 ≠ l2 = compare l1 l2
        | s1 == s2 = compare (S ss1) (S ss2)
        | s1 > s2 = GT
        | otherwise = LT
        where l1 = length ss1
              l2 = length ss2

unpack ∷ Seq → [Int]
unpack (S ss) = ss

answer ∷ [Seq] → [Int]
answer [] = [-1]
answer ss = unpack ∘ minimum $ ss

revfac ∷ Int → [Int] → [Seq]
revfac n as = go as 1
    where go ∷ [Int] → Int → [Seq]
          go [] _ = []
          go (a:as) c
              | p ≡ n = [S [c, p]]
              | p > n = []
              | p < n = (map (\(S ss) -> S (c:ss)) $ go (a:as) p) ⧺ go as c
              where p = a * c

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    (n:k:_) ← getInts <$> BSC.getLine
    sort ∘ getInts <$> BSC.getLine ≫= 
        BSC.putStrLn ∘ BSC.intercalate " " ∘ map (BSC.pack ∘ show) ∘ answer ∘ revfac n

