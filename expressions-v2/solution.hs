{-# LANGUAGE UnicodeSyntax, LambdaCase #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import Control.Applicative
import Data.Char
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

modulo ∷ Int
modulo = 10^9 + 7

newtype Parser a = P (String → [(a, String)])

data Expression = E Term OpAS Expression | ET Term
    deriving (Show, Eq)

data Term = T Factor OpMD Term | TF Factor
    deriving (Show, Eq)

data Factor = F Int | FS OpAS Factor | FP Expression
    deriving (Show, Eq)

data OpAS = Add | Sub
    deriving (Show, Eq)

data OpMD = Mul | Div
    deriving (Show, Eq)

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

digit ∷ Parser Char
digit = sat isDigit

number ∷ Parser Int
number = do
    space
    ns ← many digit
    return ∘ read $ ns

opmd ∷ Parser OpMD
opmd = do
    symbol "*"
    return Mul
    <|> do
    symbol "/"
    return Div

opas ∷ Parser OpAS
opas = do
    symbol "+"
    return Add
    <|> do
    symbol "-"
    return Sub

factor ∷ Parser Factor
factor = do
    o ← opas
    FS o <$> factor
    <|> do
    symbol "("
    e ← expr
    symbol ")"
    return $ FP e
    <|> F <$> number

term ∷ Parser Term
term = do
    f ← factor
    o ← opmd
    T f o <$> term
    <|> TF <$> factor

expr ∷ Parser Expression
expr = do
    t ← term
    o ← opas
    E t o <$> expr
    <|> ET <$> term

eval ∷ Expression → Int
eval (E t Add e) = (evalT t + eval e) `mod` modulo
eval (E t Sub e) = (evalT t + (((-1) * eval e) `mod` modulo)) `mod` modulo
eval (ET t) = evalT t `mod` modulo

evalT ∷ Term → Int
evalT (T f Mul t) = (evalF f * evalT t) `mod` modulo
evalT (T f Div t) = (evalF f * inverse (evalT t) modulo) `mod` modulo
evalT (TF f) = evalF f `mod` modulo

evalF ∷ Factor → Int
evalF (F n) = n `mod` modulo
evalF (FS Add f) = evalF f
evalF (FS Sub f) = ((-1) * evalF f) `mod` modulo
evalF (FP e) = eval e

-- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
inverse ∷ Int → Int → Int
inverse a m = go 0 1 m a
    where go t t' r r'
              | r' ≡ 0 ∧ r > 1 = 0 -- not invertable
              | r' ≡ 0 ∧ t < 0 = t + m
              | r' ≡ 0 = t
              | otherwise = go t' (t - q * t') r' (r - q * r')
              where q = r `div` r'

main ∷ IO()
-- main = BSC.pack ∘ show ∘ parse expr ∘ BSC.unpack <$> BSC.getLine ≫=
main = BSC.pack ∘ show ∘ eval ∘ fst ∘ head ∘ parse expr ∘ BSC.unpack <$> BSC.getLine ≫=
       BSC.putStrLn

