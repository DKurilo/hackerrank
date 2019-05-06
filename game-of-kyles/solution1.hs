{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.List (sort, nub, splitAt)
import qualified Data.Set as DS
import Data.Bits
import Debug.Trace
import System.IO

data Answer = WIN | LOSE
    deriving (Show, Eq)

removeP ∷ Int → Int → [[Int]]
removeP p n = [[k | k > 0] ⧺ [y | y ← [n - k - p], y > 0] | k ← [0..((n - p) `div` 2)]]

breakT ∷ Int → [[Int]]
breakT n = removeP 1 n ⧺ removeP 2 n

next ∷ [Int] → DS.Set [Int]
next ps = DS.fromList ∘ foldr (\n ts → let (psb, p:psa) = splitAt n ps in
                               (map (\t → mergeS psb $ t ⧺ psa) ∘ breakT $ p) ⧺ ts) [] $
                               [0..(length ps - 1)]

prev ∷ [Int] → DS.Set [Int]
prev ps = DS.fromList $ (1:ps):mergeS [2] ps:
          (join ∘ map (\n → let (psb, p:psa) = splitAt n ps in
                            [psb ⧺ mergeS [p+1+head psa] (tail psa) | not ∘ null $ psa]
                            ⧺ [psb ⧺ mergeS [p+2+head psa] (tail psa) | not ∘ null $ psa]
                            ⧺ [psb ⧺ mergeS [p+1] psa
                            , psb ⧺ mergeS [p+2] psa]) $ [0..(length ps - 1)])

mergeS ∷ [Int] → [Int] → [Int]
mergeS [] bs = bs
mergeS as [] = as
mergeS (a:as) (b:bs)
    | a ≤ b = a:mergeS as (b:bs)
    | otherwise = b:mergeS (a:as) bs

buildS ∷ Int → DS.Set [Int]
buildS = go init init
    where init = DS.fromList [[1],[2]]
          go ∷ DS.Set [Int] → DS.Set [Int] → Int → DS.Set [Int]
          go _ _ 0 = DS.empty
          go _ _ (-1) = DS.empty
          go ets lts n = trace (show $ length lts) $  DS.union mts $ go (DS.union ets lts') lts' (n - 2)
              where mts = DS.unions ∘ DS.map (
                            DS.filter ((≡DS.singleton True) ∘ DS.map(`DS.member` ets) ∘ next) ∘
                            prev) $ lts
                    lts' = DS.unions ∘ DS.map prev $ mts

check ∷ DS.Set [Int] → [Int] → Answer
check s ps = if ps `DS.member` s then LOSE else WIN

main ∷ IO()
main = do
    let lose = buildS 30
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    t ← getInt <$> BSC.getLine

    forM_ [1..t] $ \_ → do
        n ← getInt <$> BSC.getLine
        BSC.pack ∘ show ∘ check lose ∘ sort ∘ map BSC.length ∘ filter (not ∘ BSC.null) ∘
            BSC.split 'X' <$> BSC.getLine ≫= BSC.putStrLn

