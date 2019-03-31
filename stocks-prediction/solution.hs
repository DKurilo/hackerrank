{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.Array
import Debug.Trace
import System.IO

data Point = P (Array Int Int) (Array Int Int) -- left right

buildM ∷ Int → [Int] → Array Int Point
buildM n as = listArray (0, (n-1)) $ buildA as []

buildA ∷ [Int] → [Int] → [Point]
buildA [] _ = []
buildA (a:as) bs = P (arr bs) (arr as):buildA as (a:bs)
    where arr ∷ [Int] → Array Int Int
          arr [] = listArray (0, -1) []
          arr xs = listArray (0, k) $ xs'
              where (k, xs') = calc (-1) xs
                    calc ∷ Int → [Int] → (Int, [Int])
                    calc _ [] = (-1, [])
                    calc pd (y:ys)
                        | y < a = (-1, [])
                        | otherwise = (1 + j, d:ds)
                        where d = max pd (y - a)
                              (j, ds) = calc d ys

findL ∷ Array Int Point → Int → Int → Int
findL as d m = 1 + find m right (bounds right) + find m left (bounds left)
    where (P left right) = as ! d
          find ∷ Int → Array Int Int → (Int, Int) → Int
          find _ _ (0, -1) = 0
          find tr xs (b, e)
              | (e - b) ≤ 1 ∧ xs ! b > tr ∧ xs ! e > tr = b
              | (e - b) ≤ 1 ∧ xs ! b ≤ tr ∧ xs ! e > tr = b + 1
              | (e - b) ≤ 1 = e + 1
              | xs ! m > tr = find tr xs (b, m)
              | otherwise = find tr xs (m, e)
              where m = (b + e) `div` 2

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    n ← getInt <$> BSC.getLine
    as ← getInts <$> BSC.getLine
    let ar = listArray (0,n) as
    let ps = buildM n as
    let ma = minimum as
    let mm = maximum as - ma
    q ← getInt <$> BSC.getLine

    ss ← forM [1..q] $ \_ → do
        (d:m:_) ← getInts <$> BSC.getLine
        return ∘ BSC.pack ∘ show $ if ar ! d ≡ ma ∧ m ≥ mm then n else findL ps d m

    BSC.putStrLn ∘ BSC.intercalate "\n" $ ss

