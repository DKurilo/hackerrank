{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

data Tree = Node Int [Tree]
    deriving (Show)

data Cursor = C Tree [Int]
    deriving (Show)

action ∷ BSC.ByteString → ([Int], Cursor) → ([Int], Cursor)
action bs 
    | c ≡ "change" = set (ai 0)
    | c ≡ "print" = val
    | c ≡ "visit" ∧ as 0 ≡ "left" = left
    | c ≡ "visit" ∧ as 0 ≡ "right" = right
    | c ≡ "visit" ∧ as 0 ≡ "parent" = parent
    | c ≡ "visit" ∧ as 0 ≡ "child" = child (ai 1)
    | c ≡ "insert" ∧ as 0 ≡ "right" = insertRight (ai 1)
    | c ≡ "insert" ∧ as 0 ≡ "left" = insertLeft (ai 1)
    | c ≡ "insert" ∧ as 0 ≡ "child" = insertChild (ai 1)
    | c ≡ "delete" = delete
    | otherwise = noop
    where (c:args) = BSC.split ' ' bs ⧺ ["0","0"]
          ai = getInt ∘ (!!) args
          as = (args !!)

set ∷ Int → ([Int], Cursor) → ([Int], Cursor)
set v' (as, C t cs) = (as, C t' cs)
    where t' = go t cs
          go (Node _ ts) [] = Node v' ts
          go (Node v ts) (c:cs') = Node v (bts ⧺ [go ct cs'] ⧺ ats)
              where bts = take (c - 1) ts
                    (ct:ats) = drop (c - 1) ts

val ∷ ([Int], Cursor) → ([Int], Cursor)
val (as, C t cs) = (v:as, C t cs)
    where v = go t cs
          go (Node v ts) [] = v
          go (Node v ts) (c:cs') = go ct cs'
              where ct = head ∘ drop (c - 1) $ ts

delete ∷ ([Int], Cursor) → ([Int], Cursor)
delete (as, C t cs) = (as, C t' (reverse ∘ tail ∘ reverse $ cs))
    where t' = go t cs
          go (Node v ts) (c:[]) = Node v (take (c - 1) ts ⧺ drop c ts)
          go (Node v ts) (c:cs') = Node v (bts ⧺ [go ct cs'] ⧺ ats)
              where bts = take (c - 1) ts
                    (ct:ats) = drop (c - 1) ts

insertRight ∷ Int → ([Int], Cursor) → ([Int], Cursor)
insertRight v' (as, C t cs) = (as, C t' cs)
    where t' = go t cs
          go (Node v ts) (c:[]) = Node v (take c ts ⧺ [Node v' []] ⧺ drop c ts)
          go (Node v ts) (c:cs') = Node v (bts ⧺ [go ct cs'] ⧺ ats)
              where bts = take (c - 1) ts
                    (ct:ats) = drop (c - 1) ts

insertLeft ∷ Int → ([Int], Cursor) → ([Int], Cursor)
insertLeft v' (as, C t cs) = (as, C t' (reverse (rc + 1:rcs)))
    where t' = go t cs
          rc:rcs = reverse cs
          go (Node v ts) (c:[]) = Node v (take (c - 1) ts ⧺ [Node v' []] ⧺ drop (c - 1) ts)
          go (Node v ts) (c:cs') = Node v (bts ⧺ [go ct cs'] ⧺ ats)
              where bts = take (c - 1) ts
                    (ct:ats) = drop (c - 1) ts

insertChild ∷ Int → ([Int], Cursor) → ([Int], Cursor)
insertChild v' (as, C t cs) = (as, C t' cs)
    where t' = go t cs
          go (Node v ts) [] = Node v (Node v' []:ts)
          go (Node v ts) (c:cs') = Node v (bts ⧺ [go ct cs'] ⧺ ats)
              where bts = take (c - 1) ts
                    (ct:ats) = drop (c - 1) ts

left ∷ ([Int], Cursor) → ([Int], Cursor)
left (as, C t cs) = (as, C t (reverse (rc - 1:rcs)))
    where rc:rcs = reverse cs

right ∷ ([Int], Cursor) → ([Int], Cursor)
right (as, C t cs) = (as, C t (reverse (rc + 1:rcs)))
    where rc:rcs = reverse cs

parent ∷ ([Int], Cursor) → ([Int], Cursor)
parent (as, C t cs) = (as, C t (reverse ∘ tail ∘ reverse $ cs))

child ∷ Int → ([Int], Cursor) → ([Int], Cursor)
child c (as, C t cs) = (as, C t (cs ⧺ [c]))

noop ∷ ([Int], Cursor) → ([Int], Cursor)
noop x = x

getInt ∷ BSC.ByteString → Int
getInt bx = case BSC.readInt bx of
    Just (x,_) → x
    _ → 0

main ∷ IO()
main = do

    let getInts = map getInt <$> BSC.split ' '

    n ← getInt <$> BSC.getLine
    as ← forM [1..n] $ \_ → action <$> BSC.getLine

    let (qs, _) = foldl (\x a → a x) ([],C (Node 0 []) []) as

    BSC.putStrLn ∘ BSC.intercalate "\n" ∘ map (BSC.pack ∘ show) ∘ reverse $ qs

