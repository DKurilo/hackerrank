{-# LANGUAGE UnicodeSyntax, LambdaCase #-}
-- http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm
module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import Control.Applicative
import Data.Char
import qualified Data.ByteString.Char8 as BSC
import Data.Ord (Ordering)
import Data.List (sort)
import Debug.Trace
import System.IO

newtype Parser a = P (String → [(a, String)])

data Expr = V Var | Appl Expr Expr | Abst Var Expr | S | K | I | B | C | T Expr
    deriving (Eq)

instance Show Expr where
    show (V v) = v
    show (Appl e1 e2)
        | isSimple e2 = show e1 ⧺ show e2
        | otherwise = show e1 ⧺ "(" ⧺ show e2 ⧺ ")"
    show (Abst v e)
        | isSimple e = "\\" ⧺ v ⧺ ". " ⧺ show e
        | otherwise = "\\" ⧺ v ⧺ ". " ⧺ "(" ⧺ show e ⧺ ")"
    show S = "S"
    show K = "K"
    show I = "I"
    show B = "B"
    show C = "C"
    show (T e) = " T[" ⧺ show e ⧺ "] "

isSimple ∷ Expr → Bool
isSimple (V _) = True
isSimple (Appl _ _) = False
isSimple (Abst _ _) = False
isSimple S = True
isSimple K = True
isSimple I = True
isSimple B = True
isSimple C = True
isSimple (T _) = True

type Var = String

parse ∷ Parser a → String → [(a, String)]
parse (P p) = p

item ∷ Parser Char
item = P (\case {[] → [];(x:xs) → [(x, xs)]})

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
    empty = P (const [])
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

buildAbst ∷ [Var] → Expr → Expr
buildAbst [v] e = Abst v e
buildAbst (v:vs) e = Abst v $ buildAbst vs e

buildAppl ∷ [Expr] → Expr
buildAppl [e] = e
buildAppl es = Appl (buildAppl ∘ reverse $ es') e'
    where (e':es') = reverse es

var ∷ Parser Var
var = do
    space
    some ∘ sat $ (\c → isAlphaNum c ∨ c ≡ '_')

expr ∷ Parser Expr
expr = do
    symbol "("
    symbol "\\"
    vs ← some var
    symbol "."
    e ← expr
    symbol ")"
    return $ buildAbst vs e
    <|> do
    symbol "("
    es ← some expr
    symbol ")"
    return $ buildAppl es
    <|> do
    V <$> var

free ∷ Var → Expr → Bool
free v (V v')
    | v ≡ v' = True
    | otherwise = False
free v (Appl e1 e2) = free v e1 ∨ free v e2
free v (Abst v' e)
    | v ≡ v' = False -- error?
    | otherwise = free v e
free v (T e) = free v e
free _ _ = False

substitute ∷ Expr → (Expr, Bool)
substitute (T e) = substitute' $ T (simplify' e)
substitute (Appl e1 e2) = substitute' $ Appl (simplify' e1) (simplify' e2)
substitute (Abst v e) = substitute' $ Abst v (simplify' e)
substitute e = (e, False)

substitute' ∷ Expr → (Expr, Bool)
substitute' (T (Abst v (Appl e (V v')))) -- reduce
    | v ≡ v' ∧ not (v `free` e) = (T e, True)
substitute' (T (V v)) = (V v, False)
substitute' (T S) = (S, False)
substitute' (T K) = (K, False)
substitute' (T I) = (I, False)
substitute' (T B) = (B, False)
substitute' (T C) = (C, False)
substitute' (T (Appl e1 e2)) = (Appl (T e1) (T e2), True)
substitute' (T (Abst v e))
    | not (v `free` e) = (Appl K (T e), True)
substitute' (T (Abst v (V v')))
    | v ≡ v' = (I, False)
substitute' (T (Abst v1 (Abst v2 e)))
    | v1 `free` e = (T (Abst v1 (T (Abst v2 e))), True)
substitute' (T (Abst v (Appl e1 e2)))
    | free1 ∧ free2 = (Appl (Appl S (T (Abst v e1))) (T (Abst v e2)), True)
    | free1 = (Appl (Appl C (T (Abst v e1))) (T e2), True)
    | free2 = (Appl (Appl B (T e1)) (T (Abst v e2)), True)
    where free1 = v `free` e1
          free2 = v `free` e2
substitute' e = (e, False)

simplify ∷ Expr → Expr
simplify e = simplify' $ T e

simplify' ∷ Expr → Expr
simplify' e
    | ch = simplify' e'
    | otherwise = e'
    where (e', ch) = substitute e

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0
    t ← getInt <$> BSC.getLine
    forM_ [1..t] $ \_ →
        -- BSC.pack ∘ show ∘ parse expr ∘ BSC.unpack <$> BSC.getLine ≫=
        BSC.pack ∘ show ∘ simplify ∘ fst ∘ head ∘ parse expr ∘ BSC.unpack <$> BSC.getLine ≫=
            BSC.putStrLn

