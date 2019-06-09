{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Control.Monad.Unicode
import Data.Char (ord, chr)
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.Environment
import System.IO

allowed ∷ String
allowed = "><+-.,[]`"

maxc ∷ Int
maxc = 10^10

data Op = IncDP | DecDP | IncD | DecD | Write | Read | JFZ | JBNZ | Debug

parseOp ∷ Char → Op
parseOp '>' = IncDP
parseOp '<' = DecDP
parseOp '+' = IncD
parseOp '-' = DecD
parseOp '.' = Write
parseOp ',' = Read
parseOp '[' = JFZ
parseOp ']' = JBNZ
parseOp '`' = Debug

data Program = P [Op] Op [Op]

nexto ∷ Program → Maybe Program
nexto (P pos o []) = Nothing
nexto (P pos o (o':nos)) = Just $ P (o:pos) o' nos

prevo ∷ Program → Maybe Program
prevo (P [] o nos) = Nothing
prevo (P (o':pos) o nos) = Just $ P pos o' (o:nos)

nextm ∷ Program → Maybe Program
nextm p = go (nexto p) 0
    where go Nothing _ = Nothing
          go (Just p@(P _ JBNZ _)) 0 = Just p
          go (Just p@(P _ JFZ _)) c = go (nexto p) (c+1)
          go (Just p@(P _ JBNZ _)) c = go (nexto p) (c-1)
          go (Just p) c = go (nexto p) c

prevm ∷ Program → Maybe Program
prevm p = go (prevo p) 0
    where go Nothing _ = Nothing
          go (Just p@(P _ JFZ _)) 0 = Just p
          go (Just p@(P _ JBNZ _)) c = go (prevo p) (c+1)
          go (Just p@(P _ JFZ _)) c = go (prevo p) (c-1)
          go (Just p) c = go (prevo p) c

data Data = D [Int] Int [Int]

opd ∷ (Int → Int) → Data → Data
opd o (D pcs c ncs) = D pcs c'' ncs
    where c' = o c
          c'' | c' < 0 = 255
              | c' > 255 = 0
              | otherwise = c'

nextd ∷ Data → Data
nextd (D pcs c (c':ncs)) = D (c:pcs) c' ncs

prevd ∷ Data → Data
prevd (D (c':pcs) c ncs) = D pcs c' (c:ncs)

readd ∷ Data → String
readd (D _ c _) = [chr c]

showd ∷ Data → String
showd (D _ c _) = show c

writed ∷ Char → Data → Data
writed c (D pcs _ ncs) = D pcs (ord c)  ncs

type Input = String

type Counter = Int

data Machine = M Counter Program Data Input

buildMachine ∷ Int → String → String → Machine
buildMachine c ocs = M c p d
    where (o: os) = map parseOp ocs
          p = P [] o os
          d = D (repeat 0) 0 (repeat 0)

process ∷ Machine → String
process (M 0 _ _ _) = "\nPROCESS TIME OUT. KILLED!!!"
process (M c p d i) = case step p d i of
    (Just (p', d', i'), out) → out ⧺ process (M (c-1) p' d' i')
    (Nothing, out) → out

step ∷ Program → Data → Input → (Maybe (Program, Data, Input), String)
step p@(P _ IncDP _) d i = case nexto p of
    Just p' → (Just (p', nextd d, i), "")
    _ → (Nothing, "")
step p@(P _ DecDP _) d i = case nexto p of
    Just p' → (Just (p', prevd d, i), "")
    _ → (Nothing, "")
step p@(P _ IncD _) d i = case nexto p of
    Just p' → (Just (p', opd (+1) d, i), "")
    _ → (Nothing, "")
step p@(P _ DecD _) d i = case nexto p of
    Just p' → (Just (p', opd (+(-1)) d, i), "")
    _ → (Nothing, "")
step p@(P _ Write _) d i = case nexto p of
    Just p' → (Just (p', d, i), readd d)
    _ → (Nothing, readd d)
step p@(P _ Read _) d [] = (Nothing, "")
step p@(P _ Read _) d (c:i) = case nexto p of
    Just p' → (Just (p', writed c d, i), "")
    _ → (Nothing, "")
step p@(P _ JFZ _) d@(D _ 0 _) i = case nextm p of
    Just p' → (Just (p', d, i), "")
    _ → (Nothing, "")
step p@(P _ JFZ _) d i = case nexto p of
    Just p' → (Just (p', d, i), "")
    _ → (Nothing, "")
step p@(P _ JBNZ _) d@(D _ 0 _) i = case nexto p of
    Just p' → (Just (p', d, i), "")
    _ → (Nothing, "")
step p@(P _ JBNZ _) d i = case prevm p of
    Just p' → (Just (p', d, i), "")
    _ → (Nothing, "")
step p@(P _ Debug _) d i = case nexto p of
    Just p' → (Just (p', d, i), showd d)
    _ → (Nothing, showd d)

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    (n:m:_) ← getInts <$> BSC.getLine
    input ← take n <$> getLine
    prog ← filter (`elem` allowed) ∘ join <$> forM [1..m] (\_ → BSC.unpack <$> BSC.getLine)

    let m = buildMachine maxc prog input
    BSC.putStrLn ∘ BSC.pack ∘ process $ m

