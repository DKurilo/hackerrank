{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode -- so no we can use ←, ∷, etc
import Control.Monad.Unicode -- so now we can use ≫, ≫=, etc.
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as DM
import Data.List (sort, sortBy)
import Debug.Trace
import System.IO

data Strand = S Int Int String
data Trie a b c = El (DM.Map b c) (DM.Map a (Trie a b c)) | Fin (DM.Map b c)
    deriving (Show)

getvs ∷ Trie a b c → DM.Map b c
getvs (El vs _) = vs
getvs (Fin vs) = vs

gettm ∷ Trie a b c → DM.Map a (Trie a b c)
gettm (El _ tm) = tm
gettm (Fin _) = DM.empty

instance (Ord a, Ord b) ⇒ Semigroup (Trie a b c) where
    Fin xs <> Fin ys = Fin (DM.union xs ys)
    Fin xs <> El ys ytm = El (DM.union xs ys) ytm
    El xs xtm <> Fin ys = El (DM.union xs ys) xtm
    El xs xtm <> El ys ytm =
        El (DM.union xs ys) (DM.unionWith (<>) xtm ytm)

health ∷ Trie Char Int Int → Strand → Int
health gs (S s e cs) = go cs [gs]
    where go ∷ String → [Trie Char Int Int] → Int
          go "" ts = sumt ts
          go cs [] = error "impossible in health"
          go (c:cs) ts = sumt ts + go cs (gs:(join ∘ map (nextts c)) ts)
          sumt = sum ∘ map (sumh ∘ getvs)
          nextts c t = case DM.lookup c (gettm t) of
                           Just t → [t]
                           _ → []
          sumh vs = case (DM.lookupLT s vs, DM.lookupLE e vs) of
                        (Just (_,hs), Just (_,he)) → he - hs
                        (Nothing, Just (_,he)) → he
                        _ → 0

minmax ∷ (Ord a) ⇒ [a] → (a,a)
minmax [] = error "empty list: minmax"
minmax (x:xs) = foldr (\a (mn,mx) → (min mn a,max mx a)) (x,x) xs

substrs ∷ String → [String]
substrs = go ""
    where go _ [] = []
          go _ [_] = []
          go ccs (c:cs) = ncs:go ncs cs
              where ncs = c:ccs

buildTrie ∷ [(String, Int, Int)] → Trie Char Int Int
buildTrie = foldr addGene (Fin DM.empty)

addGene ∷ (String, Int, Int) → Trie Char Int Int → Trie Char Int Int
addGene ([c], i, h) (El vs tm) =
    El vs (DM.insertWith (<>) c (Fin (DM.singleton i h)) tm)
addGene ([c], i, h) (Fin vs) =
    El vs (DM.singleton c (Fin (DM.singleton i h)))
addGene (c:cs, i, h) (El vs tm) =
    El vs (DM.insertWith (<>) c (addGene (cs, i, h) (Fin DM.empty)) tm)
addGene (c:cs, i, h) (Fin vs) =
    El vs (DM.singleton c (addGene (cs, i, h) (Fin DM.empty)))
addGene ("", _, _) t = t

incHealth ∷ [(String, Int, Int)] → [(String, Int, Int)]
incHealth = (\(x,_,_) → x) ∘
            foldl (\(gs, ch, cg) (ns, i, h) → if cg ≡ ns
                                                then ((ns, i, ch+h):gs, ch+h, cg)
                                                else ((ns, i, h):gs, h, ns)) ([], 0, "")

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '
    n ← getInt <$> BSC.getLine
    ns ← map BSC.unpack . BSC.split ' ' <$> BSC.getLine
    hs ← getInts <$> BSC.getLine
    let gs = buildTrie ∘ sortBy (\(_,i1,_) (_,i2,_) → compare i1 i2) ∘ incHealth ∘ sort $
             zip3 ns [0..] hs
    
    q ← getInt <$> BSC.getLine
    sts ← forM [1..q] $ \_ → do
        (ss:se:cs:_) ← BSC.split ' ' <$> BSC.getLine
        return $ S (getInt ss) (getInt se) (BSC.unpack cs)

    BSC.putStrLn ∘ BSC.pack ∘ (\(x,y) → show x ++ " " ++ show y) ∘ minmax ∘ map (health gs) $ sts
