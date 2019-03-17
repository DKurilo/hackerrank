{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import qualified Data.List as DL
import System.Environment
import System.IO

data BTree = Empty | Node Int BTree BTree | Tip Int
    deriving (Show, Eq)

empty ∷ BTree
empty = Empty

singleton ∷ Int → BTree
singleton n = singleton' n lbs bs'
    where bs' = bin 0 n
          lbs = length bs'

singleton' ∷ Int → Int → [Bool] → BTree
singleton' n 0 [] = Tip n
singleton' n d (b:bs)
    | b = Node d empty (singleton' n (d-1) bs)
    | otherwise = Node d (singleton' n (d-1) bs) empty

depth ∷ BTree → Int
depth t = case t of
    Empty → 0
    Tip n → length ∘ bin 0 $ n
    Node d _ _ → d

insert ∷ Int → BTree → BTree
insert n t
    | t ≡ Empty = singleton n
    | otherwise = go lbs bs t
    where d = depth t
          bs = bin d n
          lbs = length bs
          go ∷ Int → [Bool] → BTree → BTree
          go _ _ (Tip k) = Tip k
          go cd [] Empty = Tip n
          go cd bs Empty = singleton' n cd bs
          go cd (b:bs) t
              | cd > dt ∧ b = Node cd (grow t (cd-1)) (singleton' n (cd-1) bs)
              | cd > dt ∧ not b = Node cd (go (cd-1) bs t) empty
              | cd ≡ dt ∧ b = Node cd tf (go (cd-1) bs tt)
              | cd ≡ dt ∧ not b = Node cd (go (cd-1) bs tf) tt
              | otherwise = Tip (-1) -- error
              where (Node _ tf tt) = t
                    dt = depth t

grow ∷ BTree → Int → BTree
grow t d
    | depth t ≥ d = t
    | otherwise = Node d (grow t $ d-1) empty

fromList ∷ [Int] → BTree
fromList = DL.foldl' (\t n → insert n t) empty

bin ∷ Int -> Int → [Bool]
bin 0 0 = [False]
bin l 0 = take l ∘ repeat $ False
bin l 1 = (take (l - 1) ∘ repeat $ False) ⧺ [True]
bin l n = bin (l-1) d ⧺ bin 1 m
    where (d,m) = divMod n 2

dec ∷ [Bool] → Int
dec n = dec' ∘ reverse $ n

dec' ∷ [Bool] → Int
dec' [] = 0
dec' (c:cs)
    | not c = 2 * dec' cs
    | otherwise = 1 + 2 * dec' cs

maxXors ∷ [Int] → [Int] → [Int]
maxXors as qs = map (maxXor t ∘ bin dt) qs
    where t = fromList as
          dt = depth t

maxXor ∷ BTree → [Bool] → Int
maxXor t bs = dec $ go t bs lbs
    where lbs = length bs
          go ∷ BTree → [Bool] → Int → [Bool]
          go t' [] d = [] -- error
          go Empty _ _ = []
          go (Tip _) _ _ = []
          go t' (b:bs) d
              | d > dt = b:go t' bs (d-1)
              | d ≡ dt ∧ b = case t' of
                  Node _ (Tip _) _ → [True]
                  Node _ Empty tt → False: go tt bs (d-1)
                  Node _ tf _ → True: go tf bs (d-1)
              | d ≡ dt ∧ not b = case t' of
                  Node _ _ (Tip _) → [True]
                  Node _ tf Empty → False: go tf bs (d-1)
                  Node _ _ tt → True: go tt bs (d-1)
              where dt = depth t'

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    n ← getInt <$> BSC.getLine
    as ← getInts <$> BSC.getLine

    m ← getInt <$> BSC.getLine
    qs ← forM [1..m] $ \_ → getInt <$> BSC.getLine

    let ans =  maxXors as qs

    BSC.putStrLn $ BSC.intercalate "\n" $ map (BSC.pack ∘ show) ans
    -- BSC.hPutStrLn fptr $ BSC.intercalate "\n" $ map (BSC.pack ∘ show) ans

    hFlush fptr
    hClose fptr

