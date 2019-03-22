{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Debug.Trace
import System.Environment
import System.IO

data Val = X | O
    deriving (Eq)
instance Show Val where
    show X = "X"
    show O = "."

data Tree = Node Tree Val Tree | Tip Val
    deriving (Eq, Show)

newtype Parser a = P (String → [(a, String)])

parse ∷ Parser a → String → [(a, String)]
parse (P p) inp = p inp

item ∷ Parser Char
item = P (\inp → case inp of
                     [] → []
                     (x:xs) → [(x, xs)])

instance Functor Parser where
    fmap g p = P (\inp → case parse p inp of
                             [] → []
                             [(v, out)] → [(g v, out)])

instance Applicative Parser where
    pure v = P (\inp → [(v, inp)])
    pg <*> px = P (\inp → case parse pg inp of
                              [] → []
                              [(g, out)] → parse (fmap g px) out)

instance Monad Parser where
    p >>= f = P (\inp → case parse p inp of
                            [] → []
                            [(v, out)] → parse (f v) out)

instance Alternative Parser where
    empty = P (\inp → [])
    p <|> q = P (\inp → case parse p inp of
                            [] → parse q inp
                            [(v, out)] → [(v, out)])
sat ∷ (Char → Bool) → Parser Char
sat p = do 
    x ← item
    if p x then return x else empty

char ∷ Char → Parser Char
char x = sat (≡x)

string ∷ String → Parser String
string [] = return []
string (x:xs) = do
    char x
    string xs
    return (x:xs)

space ∷ Parser ()
space = do
    many (sat isSpace)
    return ()

token ∷ Parser a → Parser a
token p = do
    space
    v ← p
    space
    return v

symbol ∷ String → Parser String
symbol xs = token (string xs)

val ∷ Parser Val
val = do
    x ← item
    if x ≡ 'X'
    then return X
    else if x ≡ '.'
    then return O
    else empty

tree ∷ Parser Tree
tree = do
    symbol "("
    l ← tree
    v ← val
    r ← tree
    symbol ")"
    return $ Node l v r
    <|> do
    v ← token val
    return $ Tip v

applySteps ∷ ([Tree], [Tree]) → [Val] → Int → ([Tree], [Tree])
appplSteps ([], _) _ _ = ([],[]) -- error! World is not created yet!
applySteps ((t:tf), tp) r s
    | s ≡ 0 = ((t:tf), tp)
    | s < 0 = applySteps (tf, t:tp) r (s + 1)
    | s > 0 ∧ tp ≠ [] = applySteps (head tp:t:tf, tail tp) r (s - 1)
    | otherwise = applySteps (applyRule r O t:t:tf, []) r (s - 1)

applyRule ∷ [Val] → Val → Tree → Tree
applyRule r p (Node tl v tr) = Node (applyRule r v tl) (r !! i) (applyRule r v tr)
    where i = (iv p) * 8 + (irval tl) * 4 + (iv v) * 2 + (irval tr)
applyRule r p (Tip v) = Tip (r !! ((iv p) * 8 + (iv v) * 2))

rval ∷ Tree → Val
rval (Tip v) = v
rval (Node _ v _) = v

irval ∷ Tree → Int
irval = iv ∘ rval

iv ∷ Val → Int
iv O = 0
iv X = 1

telem ∷ Tree → String → Val
telem t [] = rval t
telem (Node tl v tr) (d:ds)
    | d ≡ '<' = telem tl ds
    | d ≡ '>' = telem tr ds
    | otherwise = telem (Node tl v tr) ds
telem t _ = rval t

rules ∷ Int -> Int → [Val]
rules 0 0 = [O]
rules l 0 = take l ∘ repeat $ O
rules l 1 = (take (l - 1) ∘ repeat $ O) ⧺ [X]
rules l n = rules (l-1) d ⧺ rules 1 m
    where (d,m) = divMod n 2

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    r ← reverse ∘ rules 16 ∘ getInt <$> BSC.getLine
    pt ← parse tree ∘ BSC.unpack <$> BSC.getLine
    let t = case pt of
                [(t', _)] → t'
                [] → Tip O -- error
              
    q ← getInt <$> BSC.getLine
    foldM_ (\ts _ → do
        (ss:ps:_) ← BSC.split ' ' <$> BSC.getLine
        let s = getInt ss
        let p = BSC.unpack ps
        let ts' = applySteps ts r s
        let ((t':_), _) = ts'
        BSC.putStrLn ∘ BSC.pack ∘ show ∘ telem t' $ p
        return ts') ([t], []) [1..q]

