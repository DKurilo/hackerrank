-- http://web.mit.edu/sp.268/www/nim.pdf
-- http://www.math.ucla.edu/~tom/Game_Theory/comb.pdf
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.List (sort, nub, splitAt)
import Data.Bits
import Debug.Trace
import System.IO

data Answer = WIN | LOSE
    deriving (Show, Eq)

removeP ∷ Int → Int → [[Int]]
removeP p n = [[k | k > 0] ⧺ [y | y ← [n - k - p], y > 0] | k ← [0..((n - p) `div` 2)]]

breakT ∷ Int → [[Int]]
breakT n = removeP 1 n ⧺ removeP 2 n

mergeS ∷ [Int] → [Int] → [Int]
mergeS [] bs = bs
mergeS as [] = as
mergeS (a:as) (b:bs)
    | a ≤ b = a:mergeS as (b:bs)
    | otherwise = b:mergeS (a:as) bs

sg ∷ Int → Int
sg = ([sg' n | n ← [0..]] !!)
    where sg' ∷ Int → Int
          sg' 0 = 0
          sg' 1 = 1
          sg' 2 = 2
          sg' n = mex ∘ map (xorS ∘ map sg) ∘ breakT $ n

mex ∷ [Int] → Int
mex = (+1) ∘ foldl (\g x → if x - g ≤ 1 then x else g) (-1) ∘ sort

xorS ∷ [Int] → Int
xorS = foldl xor 0

check ∷ [Int] → Answer
check = (\v → if v ≡ 0 then LOSE else WIN) ∘ xorS ∘ map sg

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    t ← getInt <$> BSC.getLine

    forM_ [1..t] $ \_ → do
        n ← getInt <$> BSC.getLine
        BSC.pack ∘ show ∘ check ∘ sort ∘ map BSC.length ∘ filter (not ∘ BSC.null) ∘
            BSC.split 'X' <$> BSC.getLine ≫= BSC.putStrLn

