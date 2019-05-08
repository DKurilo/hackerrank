{-# LANGUAGE OverloadedStrings, UnicodeSyntax, LambdaCase #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import Control.Applicative
import Data.Char
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

newtype Parser a = P (String → [(a, String)])

newtype Name = N String
    deriving (Eq)
instance Show Name where
    show (N s) = s

newtype Var = Var Name
    deriving (Eq)
instance Show Var where
    show (Var n) = '#':show n

data Term = V Var | R RTerm
    deriving (Eq)
instance Show Term where
    show (V v) = show v
    show (R r) = show r

data RTerm = RT Name [Term]
    deriving (Eq)
instance Show RTerm where
    show (RT n []) = show n
    show (RT n ts) = "[" ⧺ show n ⧺ ": " ⧺
                     foldl (\cs t → if null cs then show t else cs ⧺ ", " ⧺ show t) "" ts ⧺ "]"

data Cond = Eq | Ne
    deriving (Eq)
instance Show Cond where
    show Eq = " = "
    show Ne = " /= "

data CTerm = T Term | C Cond Term Term
    deriving (Eq)
instance Show CTerm where
    show (T t) = show t
    show (C c t1 t2) = "<" ⧺ show t1 ⧺ show c ⧺ show t2 ⧺ ">"

data Op = Rule [CTerm] Term | Query [CTerm] | QUIT | Noop
    deriving (Eq)
instance Show Op where
    show (Rule [] t) = show t ⧺ "."
    show (Rule cts t) = "{(" ⧺ showCTerms cts ⧺ ") => " ⧺ show t ⧺ "}."
    show (Query cts) = "(" ⧺ showCTerms cts ⧺ ")?"
    show QUIT = "quit!"
    show Noop = ""

showCTerms ∷ [CTerm] → String
showCTerms = foldl (\cs ct → if null cs then show ct else cs ⧺ ", " ⧺ show ct) ""

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

name ∷ Parser Name
name = do
    x ← sat isAlpha
    xs ← many (sat (\c → isAlpha c ∨ c ≡ '-'))
    return $ N $ x:xs

var ∷ Parser Var
var = do
    char '#'
    Var <$> name

term ∷ Parser Term
term = V <$> var
    <|> do
    v ← name
    return $ R $ RT v []
    <|> (R <$> rterm)

terms ∷ Parser [Term]
terms = many $ term <|> do
    symbol ","
    term

rterm ∷ Parser RTerm
rterm = do
    symbol "["
    n ← name
    symbol ":"
    ts ← terms
    symbol "]"
    return $ RT n ts

cterm ∷ Parser CTerm
cterm = do
    symbol "<"
    t1 ← term
    symbol "="
    t2 ← term
    symbol ">"
    return $ C Eq t1 t2
    <|> do
    symbol "<"
    t1 ← term
    symbol "/="
    t2 ← term
    symbol ">"
    return $ C Ne t1 t2
    <|> (T <$> term)

cterms ∷ Parser [CTerm]
cterms = many $ cterm <|> do
    symbol ","
    cterm

op ∷ Parser Op
op = do
    t ← term
    symbol "."
    return $ Rule [] t
    <|> do
    symbol "{"
    cts ← getCterms
    symbol "=>"
    t ← term
    symbol "}"
    symbol "."
    return $ Rule cts t
    <|> do
    cts ← getCterms
    symbol "?"
    return $ Query cts
    <|> do
    symbol "quit!"
    return QUIT
    where getCterms = do
                        symbol "("
                        cts ← cterms
                        symbol ")"
                        return cts

main ∷ IO()
main = BSC.intercalate "\n" ∘ map (BSC.pack ∘ show) ∘ takeWhile (≠QUIT) ∘ map fst ∘ join ∘
       map (parse op ∘ BSC.unpack) ∘ BSC.split '\n' <$> BSC.getContents ≫= BSC.putStrLn

