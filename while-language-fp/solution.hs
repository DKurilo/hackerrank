{-# LANGUAGE UnicodeSyntax, LambdaCase #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import Control.Applicative
import Data.Char
import qualified Data.Map as DM
import qualified Data.ByteString.Char8 as BSC
import Data.List (intercalate)
import Debug.Trace
import System.IO

newtype Parser a = P (String → [(a, String)])

data Program = Pr Stmt Program | S Stmt
    deriving (Show)

data Stmt = Let Var AExp | If BExp Program Program | While BExp Program
    deriving (Show)

type Var = String

type Numb = Int

data OpAS = Add | Sub
    deriving (Show, Eq)

data OpMD = Mul | Div
    deriving (Show, Eq)

data OpR = Gt | Lt
    deriving (Show, Eq)

type AExp = [ASPair]

data ASPair = ASP OpAS Term
    deriving (Show)

type Term = [MDPair]

data MDPair = MDP OpMD Factor
    deriving (Show)

data Factor = FV Var | FN Numb | FU OpAS Term | FP AExp
    deriving (Show)

type BExp = [BOr]

type BOr = [BFactor]

data BFactor = BFFalse | BFTrue | BFR AExp OpR AExp | BFP BExp
    deriving (Show)

newtype State = ST (DM.Map String Numb)

instance Show State where
    show (ST m) = intercalate "\n" ∘ map (\(k,v) → k ⧺ " " ⧺ show v) ∘ DM.assocs $ m

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

bfactor ∷ Parser BFactor
bfactor = do
    symbol "false"
    return BFFalse
    <|> do
    symbol "true"
    return BFTrue
    <|> do
    e1 ← aexp
    o ← opr
    BFR e1 o <$> aexp
    <|> do
    symbol "("
    e ← bexp
    symbol ")"
    return $ BFP e

bfactors ∷ Parser [BFactor]
bfactors = do
    symbol "and"
    bands
    <|> return []

bands ∷ Parser [BFactor]
bands = do
    f ← bfactor
    fs ← bfactors
    return $ f:fs

bors ∷ Parser [BOr]
bors = do
    symbol "or"
    bexp
    <|> return []

bexp ∷ Parser BExp
bexp = do
    b ← bands
    bs ← bors
    return $ b:bs

factor ∷ Parser Factor
factor = FV <$> var
    <|> FN <$> numb
    <|> do
    o ← opas
    FU o <$> term
    <|> do
    symbol "("
    e ← aexp
    symbol ")"
    return $ FP e

mdpairs ∷ Parser [MDPair]
mdpairs = do
    o ← opmd
    f ← factor
    ps ← mdpairs
    return $ MDP o f : ps
    <|> return []

term ∷ Parser Term
term = do
    f ← factor
    ps ← mdpairs
    return $ MDP Mul f : ps

aspairs ∷ Parser [ASPair]
aspairs = do
    o ← opas
    t ← term
    ps ← aspairs
    return $ ASP o t : ps
    <|> return []

aexp ∷ Parser AExp
aexp = do
    t ← term
    ps ← aspairs
    return $ ASP Add t : ps

opr ∷ Parser OpR
opr = do
    symbol ">"
    return Gt
    <|> do
    symbol "<"
    return Lt

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

digit ∷ Parser Char
digit = sat isDigit

numb ∷ Parser Numb
numb = do
    space
    ns ← some digit
    return ∘ read $ ns

var ∷ Parser Var
var = do
    space
    some (sat isLower)

stmt ∷ Parser Stmt
stmt = do
    v ← var
    symbol ":="
    Let v <$> aexp
    <|> do
    symbol "if"
    b ← bexp
    symbol "then"
    symbol "{"
    pt ← program
    symbol "}"
    symbol "else"
    symbol "{"
    pf ← program
    symbol "}"
    return $ If b pt pf
    <|> do
    symbol "while"
    b ← bexp
    symbol "do"
    symbol "{"
    p ← program
    symbol "}"
    return $ While b p

program ∷ Parser Program
program = do
    s ← stmt
    some (symbol ";")
    Pr s <$> program
    <|> do
    s ← stmt
    many (symbol ";")
    return $ S s

evalBFactor ∷ State → BFactor → Bool
evalBFactor s BFFalse = False
evalBFactor s BFTrue = True
evalBFactor s (BFR e1 o e2)
    | o ≡ Lt ∧ n1 < n2 = True
    | o ≡ Gt ∧ n1 > n2 = True
    | otherwise = False
    where n1 = evalAExp s e1
          n2 = evalAExp s e2
evalBFactor s (BFP exp) = evalBExp s exp

evalBAnds ∷ State → [BFactor] → Bool
evalBAnds s = foldl (\b f → b ∧ evalBFactor s f) True

evalBExp ∷ State → BExp → Bool
evalBExp s = foldl (\b bfs → b ∨ evalBAnds s bfs) False

evalFactor ∷ State → Factor → Int
evalFactor s@(ST m) (FV vn) = m DM.! vn
evalFactor s (FN n) = n
evalFactor s (FU Add t) = evalTerm s t
evalFactor s (FU Sub t) = - evalTerm s t
evalFactor s (FP exp) = evalAExp s exp

evalTerm ∷ State → Term → Int
evalTerm s = foldl (\n (MDP o f) → let n' = evalFactor s f in case o of
        Mul → n * n'
        Div → n `div` n') 1

evalAExp ∷ State → AExp → Int
evalAExp s = foldl (\n (ASP o t) → let n' = evalTerm s t in case o of
        Add → n + n'
        Sub → n - n') 0

evalSt ∷ State → Stmt → State
evalSt s@(ST m) (Let vn exp) = ST $ DM.insert vn (evalAExp s exp) m
evalSt s (If exp pt pf) = if evalBExp s exp then eval' s pt else eval' s pf
evalSt s st@(While exp p) = if evalBExp s exp then evalSt (eval' s p) st else s

eval ∷ Program → State
eval = eval' $ ST DM.empty

eval' ∷ State → Program → State
eval' s (S st) = evalSt s st
eval' s (Pr st pr) = eval' (evalSt s st) pr

main ∷ IO()
-- main = BSC.pack ∘ show ∘ parse program ∘ BSC.unpack <$> BSC.getContents ≫=
main = BSC.pack ∘ show ∘ eval ∘ fst ∘ head ∘ parse program ∘ BSC.unpack <$> BSC.getContents ≫=
    BSC.putStrLn

