{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Data.Ord
import Data.List
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

data Sum = S Int Int Int -- sum start length
    deriving (Eq)

instance Ord Sum where
    compare (S s1 p1 l1) (S s2 p2 l2)
        | s1 ≡ s2 ∧ p1 ≡ p2 ∧ l1 ≡ l2 = EQ
        | s1 > s2 ∨ (s1 ≡ s2 ∧ (p1 < p2 ∨ (p1 ≡ p2 ∧ l1 < l2))) = GT
        | otherwise = LT

instance Show Sum where
    show (S s _ _) = show s

add ∷ Int → Sum → Sum
add n (S s p l) = S (s + n) p (l + 1)

findSums ∷ [Int] → [Sum]
findSums as = reverse ∘ sort ∘ findSums' as 1 [] $ if head as > 0 then [(S 0 1 0)] else []

findSums' ∷ [Int] → Int → [Sum] → [Sum] → [Sum]
findSums' [] _ fs _ = fs
findSums' (a:[]) i fs cs
    | a < 0 = fs
    | otherwise = map (add a) cs ⧺ fs
findSums' (a1:a2:as) i fs cs
    | a1 > 0 ∧ a2 < 0 = findSums' (a2:as) (i + 1) (cs' ⧺ fs) cs'
    | a1 > 0 ∧ a2 > 0 = findSums' (a2:as) (i + 1) fs cs'
    | a1 < 0 ∧ a2 < 0 = findSums' (a2:as) (i + 1) fs cs''
    | a1 < 0 ∧ a2 > 0 = findSums' (a2:as) (i + 1) fs ((S 0 (i + 1) 0):cs'')
    where cs' = map (add a1) $ cs
          cs'' = filter (\(S s _ _) → s ≥ 0) cs'

takeSums ∷ Int → [Sum] → [Sum]
takeSums k ss = takeSums' k ss []

takeSums' ∷ Int → [Sum] → [(Int,Int)] → [Sum]
takeSums' 0 _ _ = []
takeSums' _ [] _ = []
takeSums' k ((S s p l):ss) rs
    | check = (S s p l):takeSums' (k - 1) ss ((p, p + l - 1):rs)
    | otherwise = takeSums' k ss rs
    where check = and ∘ map (\(b, e) → p > e ∨ (p + l - 1) < b) $ rs

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    (n:k:_) ← getInts <$> BSC.getLine
    as ← getInts <$> BSC.getLine

    BSC.putStrLn ∘ BSC.intercalate "\n" ∘ map (BSC.pack ∘ show) ∘ takeSums k ∘ findSums $ as

