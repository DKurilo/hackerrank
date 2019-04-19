{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.List (sort)
import Debug.Trace
import System.IO

data Pass = P Char Bool [Pass]
    deriving (Show, Eq)

type PassG = [Pass]

searchA ∷ [String] → (String → [(String, String)])
searchA ps = searchA' pg
    where pg = buildPG [] ∘ sort $ ps

searchA' ∷ PassG → String → [(String, String)]
searchA' _ [] = []
searchA' [] _ = []
searchA' pg (c:cs) = case findC c pg of
    Just (P _ end pg') → [([c], cs) | end] ⧺
                         (map (\(fcs, rcs) → (c:fcs, rcs)) $ searchA' pg' cs)
    _ → []

findC ∷ Char → PassG → Maybe Pass
findC _ [] = Nothing
findC c ((P c' end pg'):pg)
    | c ≡ c' = Just (P c' end pg')
    | otherwise = findC c pg

buildPG ∷ PassG → [String] → PassG
buildPG pg [] = pg
buildPG pg ([]:css) = buildPG pg css
buildPG [] ((c:cs):css) =
    buildPG [(P c (cs ≡ []) $ if cs ≡ [] then [] else buildPG [] [cs])] css
buildPG (P c' end pg':pg) ((c:cs):css) = case find (searchA' (P c' end pg':pg)) (c:cs) of
    Just _ → buildPG (P c' end pg':pg) css
    _ | c' ≡ c →
          buildPG ((P c' (cs ≡ [] ∨ end) $ if cs ≡ [] then pg' else buildPG pg' [cs]):pg) css
      | otherwise → buildPG (buildPG [] [c:cs] ⧺ (P c' end pg':pg)) css

-- Naive search:
-- searchA ∷ [String] → (String → [(String, String)])
-- searchA ps = \cs → let lcs = length cs in
--                    foldr (\x a → let lx = length x in
--                                  if lcs ≥ lx ∧ take lx cs ≡ x
--                                    then (x, drop lx cs):a
--                                    else a) [] ps

find ∷ (String → [(String, String)]) → String → Maybe [String]
find sa "" = Just []
find sa cs = go $ sa cs
    where go [] = Nothing
          go ((fcs,rcs):fcss) = case find sa rcs of
              Just css → Just (fcs:css)
              _ → go fcss

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    t ← getInt <$> BSC.getLine

    forM_ [1..t] $ \_ → do
        n ← getInt <$> BSC.getLine
        sa ← searchA ∘ map BSC.unpack ∘ BSC.split ' ' <$> BSC.getLine
        p ← BSC.unpack <$> BSC.getLine
        BSC.putStrLn $ case find sa p of
            Just ps → BSC.intercalate " " ∘ map BSC.pack $ ps
            _ → "WRONG PASSWORD"

