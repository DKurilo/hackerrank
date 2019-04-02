{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.List
import Data.Ord
import qualified Data.IntMap as DM
import Debug.Trace
import System.IO

data Building = B Int Int Int -- index  per day  initial height
    deriving (Show, Eq)

instance Ord Building where
    compare (B i1 k1 b1) (B i2 k2 b2)
        | k1 ≡ k2 ∧ b1 ≡ b2 ∧ i1 ≡ i2 = EQ
        | k1 > k2 ∨ k1 ≡ k2 ∧ (b1 > b2 ∨ b1 ≡ b2 ∧ i1 > i2) = GT
        | otherwise = LT

grow ∷ Int → Building → Building
grow x (B i k b) = B i k $ k * x + b

common ∷ Building → Building → Int
common (B _ k1 b1) (B _ k2 b2) = d + (if m > 0 then 1 else 0)
    where (d,m) = (b1 - b2) `divMod` (k2 - k1)

data Day = D Int Int Int -- day number   building in day   building after day
    deriving (Show)

buildD ∷ [Building] → [Day]
buildD bs = d:(go ba ∘ filter (>ba) ∘ sort $ bs)
    where (d, bd, ba) = get0D bs
          go _ [] = []
          go (B ip kp bp) ((B i k b):[]) = [D d' id i]
              where d' = common (B ip kp bp) (B i k b)
                    id = if (kp * d' + bp) ≡ (k * d' + b) then max ip i else i
          go bp (b':bs') = case getD (common bp b') bp b' bs' of
              Just (d', bd', ba') → d':(go ba' bs')
              _ → go bp bs'

getD ∷ Int → Building → Building → [Building] → Maybe (Day, Building, Building)
getD d (B id kd bd) (B ia ka ba) bs
    | check = Nothing
    | kd * d + bd ≡ mh ∧ id > ia = Just (D d id ia, B id kd bd, B ia ka ba)
    | otherwise = Just (D d ia ia, B ia ka ba, B ia ka ba)
    where mh = ka * d + ba
          check = or ∘ map (\(B i k b) → k * d + b ≥ mh) $ bs

get0D ∷ [Building] → (Day, Building, Building)
get0D bs = (D 0 id ia, B id kd bd, B ia ka ba)
    where mh = maximum ∘ map (\(B _ _ b) → b) $ bs
          (B id kd bd) = foldl maxid (B 0 0 0) bs
          (B ia ka ba) = foldl maxk (B 0 0 0) bs
          maxid (B i1 k1 b1) (B i2 k2 b2) = if b2 ≡ mh ∧ i2 > i1
                                                then B i2 k2 b2
                                                else B i1 k1 b1
          maxk (B i1 k1 b1) (B i2 k2 b2) = if b2 ≡ mh ∧ (k2 > k1 ∨ k2 ≡ k1 ∧ i2 > i1)
                                               then B i2 k2 b2
                                               else B i1 k1 b1
findB ∷ DM.IntMap Day → Int → Int
findB dm x = case DM.lookupLE x dm of
    Just (i, D d id ia) → if x ≡ d then id else ia
    _ → 0 -- error

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    (n:q:_) ← getInts <$> BSC.getLine
    hs ← getInts <$> BSC.getLine
    es ← getInts <$> BSC.getLine

    let bs = zipWith3 (\i k b -> B i k b) [1..] es hs

    let dm = DM.fromList ∘ map (\(D d id ia) → (d,D d id ia)) ∘ buildD $ bs

    as ← forM [1..q] $ \_ → BSC.pack ∘ show ∘ findB dm ∘ getInt <$> BSC.getLine

    BSC.putStrLn $ BSC.intercalate "\n" $ as

