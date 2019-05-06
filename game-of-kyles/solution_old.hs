{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.List (sort, nub, splitAt)
import qualified Data.Map as DM
import qualified Data.Set as DS
import Debug.Trace
import System.IO

data Answer = WIN | LOSE
    deriving (Show, Eq)

na ∷ Answer → Answer
na WIN = LOSE
na LOSE = WIN

data Player = Me | Enemy
    deriving (Show, Eq, Ord)

np ∷ Player → Player
np Me = Enemy
np Enemy = Me

type Cache = DM.Map [Int] Answer

insert ∷ [Int] → Player → Answer → Cache → Cache
insert bs p a = DM.insert bs (if p ≡ Me then a else na a)

lookupA ∷ [Int] → Player → Cache → Maybe Answer
lookupA bs p c = case DM.lookup bs c of
    Just a → Just $ if p ≡ Me then a else na a
    Nothing → Nothing

removeP ∷ Int → Int → [[Int]]
removeP p n = [[k | k > 0] ⧺ [y | y ← [n - k - p], y > 0] | k ← [0..((n - p) `div` 2)]]

breakT ∷ Int → [[Int]]
breakT n = removeP 1 n ⧺ removeP 2 n

next ∷ [Int] → [[Int]]
next ps = nub ∘
           foldr (\n ts → let (psb, p:psa) = splitAt n ps in
                          (map (\t → mergeS psb $ t ⧺ psa) ∘ breakT $ p) ⧺ ts) [] $
                          [0..(length ps - 1)]

prev ∷ [Int] → [[Int]]
prev ps = nub $ (1:ps):mergeS [2] ps:
          (join ∘ map (\n → let (psb, p:psa) = splitAt n ps in
                            [psb ⧺ mergeS [p+1+head psa] (tail psa) | not ∘ null $ psa]
                            ⧺ [psb ⧺ mergeS [p+2+head psa] (tail psa) | not ∘ null $ psa]
                            ⧺ [psb ⧺ mergeS [p+1] psa
                            , psb ⧺ mergeS [p+2] psa]) $ [0..(length ps - 1)])

simplify ∷ [Int] → [Int]
simplify [] = []
simplify (0:bss) = bss
simplify [bs] = []
simplify (bs1:bs2:bss)
    | bs1 ≡ 1 ∧ bs2 ≡ 1 = simplify bss
    | otherwise = bs1:bs2:bss

mergeS ∷ [Int] → [Int] → [Int]
mergeS [] bs = bs
mergeS as [] = as
mergeS (a:as) (b:bs)
    | a ≤ b = a:mergeS as (b:bs)
    | otherwise = b:mergeS (a:as) bs

wish ∷ Player → Answer
wish Me = WIN
wish Enemy = LOSE

play ∷ Cache → Player → [Int] → (Answer, Cache)
play c p [] = (na ∘ wish $ p, c)
play c p [_] = (wish p, c)
play c p [1,1] = (na ∘ wish $ p, c)
-- play c p [1,_] = (wish p, c)
-- play c p (1:2:_) = (wish p, c)
play c p ps = case lookupA ps p c of
    Just a → (a, c)
    _ → (\(a',c') → (a', insert ps p a' c')) $
        foldr (\t (a,c') → let (a', c'') = play c' (np p) t in
                                  if null t ∨ a ≡ wish p
                                    then (wish p, c')
                                    else if a' ≡ wish p
                                      then (wish p, c'')
                                      else (a, c'')) (na ∘ wish $ p, c) ts
    where ts = next ps

buildS ∷ Int → DS.Set [Int]
buildS = DS.fromList ∘ go (DS.fromList [[1], [2]]) [[1],[2]]
    where go ∷ DS.Set [Int] → [[Int]] → Int → [[Int]]
          go _ _ 0 = []
          go _ _ (-1) = []
          go ets lts n = mts ⧺ go (DS.union ets $ DS.fromList lts') lts' (n - 2)
              where mts = filter (and ∘ map(`DS.member` ets) ∘ next) ∘ join ∘ map prev $ lts
                    lts' = join ∘ map prev $ mts

checkPs ∷ [Int] → Answer
checkPs = fst ∘ play DM.empty Me

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
        -- BSC.pack ∘ show ∘ checkPs ∘ sort ∘ map BSC.length ∘ filter (not ∘ BSC.null) ∘
        BSC.pack ∘ show ∘ check lose ∘ sort ∘ map BSC.length ∘ filter (not ∘ BSC.null) ∘
            BSC.split 'X' <$> BSC.getLine ≫= BSC.putStrLn

