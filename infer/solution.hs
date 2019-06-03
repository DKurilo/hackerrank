{-# LANGUAGE UnicodeSyntax, LambdaCase #-}
module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import Control.Exception
import Control.Applicative
import Data.Either
import Data.Char
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as DM
import Debug.Trace
import System.IO

newtype Parser a = P (String → [(a, String)])

type Ident = String

data Expr = EL Ident Expr Expr Type | EF [Ident] Expr Type | ES SExpr Type | EIT Ident Type
    deriving (Eq, Ord, Show)

data SExpr = SEP Expr | SEI Ident  | SEFC SExpr [Expr]
    deriving (Eq, Ord, Show)

data Type = Unknown | Any | TU [Type] Type | TFA [Ident] Type | TC SType Type | TST SType
    deriving (Eq, Ord, Show)

data SType = STP Type | STI Ident | STG SType [Type]
    deriving (Eq, Ord, Show)

type Table = DM.Map Ident Type

newtype TypeException = TypeException String

instance Exception TypeException

instance Show TypeException where
    show (TypeException s) = "Type error: " ⧺ s

data Token = Tok Token String

tok ∷ Int → Token
tok c = Tok tok' v
    where v = 'α':show c
          tok' = tok (c+1)

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
    ns ← some digit
    return ∘ read $ ns

list ∷ Parser a → String → Parser [a]
list p sep = do
    a ← p
    as ← rest
    return $ a:as
    <|> return []
    where rest = do
            symbol sep
            list p sep
            <|> return []

ident ∷ Parser String
ident = do
    space
    c ← sat (\c → isAlpha c ∨ c ≡ '_')
    cs ← many ∘ sat $ (\c → isAlphaNum c ∨ c ≡ '_')
    return $ c:cs

argList ∷ Parser [String]
argList = list ident ""

paramList ∷ Parser [Expr]
paramList = do
    symbol "("
    ps ← list expr ","
    symbol ")"
    return ps

curryParams ∷ SExpr → Parser SExpr
curryParams se = do
    se ← SEFC se <$> paramList
    curryParams se
    <|> return se

expr ∷ Parser Expr
expr = do
    symbol "let "
    i ← ident
    symbol "= "
    e1 ← expr
    symbol "in "
    e2 ← expr
    return $ EL i e1 e2 Unknown
    <|> do
    symbol "fun "
    as ← argList
    symbol "-> "
    e ← expr 
    return $ EF as e Unknown
    <|> do
    i ← ident
    symbol ":"
    EIT i <$> typep
    <|> do
    e ← sexpr
    return $ ES e Unknown

sexpr ∷ Parser SExpr
sexpr = do
    symbol "("
    e ← sexpr
    symbol ")"
    se ← SEFC e <$> paramList
    curryParams se
    <|> do
    i ← ident
    se ← SEFC (SEI i) <$> paramList
    curryParams se
    <|> SEI <$> ident
    <|> do
    symbol "("
    e ← expr
    symbol ")"
    return $ SEP e

typeList ∷ Parser [Type]
typeList = do
    t ← typep
    symbol ","
    ts ← list typep ","
    return $ t:ts
    <|> do
    t ← typep
    return [t]

typep ∷ Parser Type
typep = do
    symbol "() -> "
    TU [] <$> typep
    <|> do
    symbol "("
    ts ← typeList
    symbol ") -> "
    TU ts <$> typep
    <|> do
    symbol "forall["
    as ← argList
    symbol "]"
    TFA as <$> typep
    <|> do
    t ← stype
    symbol "-> "
    TC t <$> typep
    <|> TST <$> stype

stype ∷ Parser SType
stype = do
    symbol "("
    t ← typep
    symbol ")"
    stypeSpec $ STP t
    <|> do
    i ← ident
    stypeSpec $ STI i

stypeSpec ∷ SType → Parser SType
stypeSpec t = do
    symbol "["
    ts ← typeList
    symbol "]"
    stypeSpec $ STG t ts
    <|> return t

infer ∷ Table → Token → Expr → Either String (Type, Table, Token)
infer tb tk@(Tok tkn tkc) (EIT i t) = case t of
    Unknown → case DM.lookup i tb of
        Just t → Right (t, tb, tk)
        _ → Right (TST (STI i), tb, tk)
    _ → Right (t, tb, tk)
infer tb tk@(Tok tkn tkc) (ES se t) = case t of
    Unknown → case inferS tb tk se of
        Right (t, tb, tk) → Right (t, tb, tk)
        l@(Left cs) → l
    _ → Right (t, tb, tk)
infer _ _ _ = Left "Sorry, I don't know what to do with it"

inferS ∷ Table → Token → SExpr → Either String (Type, Table, Token)
inferS tb tk@(Tok tkn tkc) (SEP e) = infer tb tk e
inferS tb tk@(Tok tkn tkc) (SEI i) = case DM.lookup i tb of
    Just t → Right (t, tb, tk)
    _ → Right (TST (STI i), tb, tk)
inferS tb tk@(Tok tkn tkc) (SEFC se es) = case ets of
    Left cs → Left cs
    Right (ts, tb, tk) → case et tb tk of
        l@(Left _) → l
        Right (t, tb, tk) → case applyArgs tb tk t ts of
            r@(Right _) → r
            Left cs → Left $ cs ⧺ " " ⧺ show se
    where ets = foldr (\e ets → case ets of
                           Right (ts, tb, tk) → case infer tb tk e of
                               Left cs → Left cs
                               Right (t, tb, tk) → Right (t:ts, tb, tk)
                           l@(Left _) → l) (Right ([], tb, tk)) es
          et tb tk = inferS tb tk se

applyArgs ∷ Table → Token → Type → [Type] → Either String (Type, Table, Token)
applyArgs tb tk@(Tok tkn tkc) t [] = Right (t, tb, tk)
applyArgs tb tk@(Tok tkn tkc) Unknown (at:ats) = Left "Can't apply arguments to Unknown in"
applyArgs tb tk@(Tok tkn tkc) Any (at:ats) = Right (Any, tb, tk)
applyArgs tb tk@(Tok tkn tkc) (TFA as t) ts =
    applyArgs tb' tk' (evalType tb' tk t) (map (evalType tb' tk) ts)
    where (tb', tk') = foldl (\(tb, tk@(Tok tkn tkc)) a →
                           (DM.insert tkc Any $ DM.insert a (TST (STI tkc)) tb, tkn)) (tb, tk) as
applyArgs tb tk@(Tok tkn tkc) (TU (at:ats) t) (a:as)
    | length ats > length as = Left "Too few arguments for"
    | length ats < length as = Left "Too many arguments for"
    | (at ≡ Any ∨ at ≡ a) ∧ null ats = Right (t, tb, tk)
    | at ≡ Any ∨ at ≡ a = applyArgs tb tk (TU ats t) as
    | otherwise = trace (show (at, a)) $ Left "Wrong argument type in"
applyArgs tb tk@(Tok tkn tkc) (TC st t) (a:ats)
    | st ≡ STP Any ∨ TST st ≡ a = Right (t, tb, tk)
    | otherwise = Left "Wrong argument type in"
applyArgs tb tk@(Tok tkn tkc) t ts@(_:_) = Left "Something is wrong in"

evalType ∷ Table → Token → Type → Type
evalType tb tk@(Tok tkn tkc) Unknown = Unknown
evalType tb tk@(Tok tkn tkc) Any = Any
evalType tb tk@(Tok tkn tkc) (TU ts t) = TU (map (evalType tb tk) ts) (evalType tb tk t)
evalType tb tk@(Tok tkn tkc) (TFA is t) = TFA is (evalType tb' tk' t)
    where (tb', tk') = foldl (\(tb, Tok tkn tkc) i →
                           (DM.insert tkc Any $ DM.insert i (TST (STI tkc)) tb, tkn)) (tb, tk) is
evalType tb tk@(Tok tkn tkc) (TC st t) = TC (evalSType tb tk st) (evalType tb tk t)
evalType tb tk@(Tok tkn tkc) (TST st) = simplifyTST $ TST (evalSType tb tk st)

evalSType ∷ Table → Token → SType → SType
evalSType tb tk (STP t) = STP (evalType tb tk t)
evalSType tb tk st@(STI i) = case DM.lookup i tb of
    Just t → STP t
    _ → st
evalSType tb tk (STG st ts) = STG (evalSType tb tk st) (map (evalType tb tk) ts)

simplifyTST ∷ Type → Type
simplifyTST (TST (STP t)) = simplifyTST t
simplifyTST t = t

loadEnv ∷ [String] → Table
loadEnv = foldl (\tb cs → case fst ∘ head ∘ parse expr $ cs of
        EIT i t → DM.insert i t tb
        _ → tb) DM.empty

env ∷ [String]
env = [ "head: forall[a] list[a] -> a"
      , "tail: forall[a] list[a] -> list[a]"
      , "nil: forall[a] list[a]"
      , "cons: forall[a] (a, list[a]) -> list[a]"
      , "cons_curry: forall[a] a -> list[a] -> list[a]"
      , "map: forall[a b] (a -> b, list[a]) -> list[b]"
      , "map_curry: forall[a b] (a -> b) -> list[a] -> list[b]"
      , "one: int"
      , "zero: int"
      , "succ: int -> int"
      , "plus: (int, int) -> int"
      , "eq: forall[a] (a, a) -> bool"
      , "eq_curry: forall[a] a -> a -> bool"
      , "not: bool -> bool"
      , "true: bool"
      , "false: bool"
      , "pair: forall[a b] (a, b) -> pair[a, b]"
      , "pair_curry: forall[a b] a -> b -> pair[a, b]"
      , "first: forall[a b] pair[a, b] -> a"
      , "second: forall[a b] pair[a, b] -> b"
      , "id: forall[a] a -> a"
      , "const: forall[a b] a -> b -> a"
      , "apply: forall[a b] (a -> b, a) -> b"
      , "apply_curry: forall[a b] (a -> b) -> a -> b"
      , "choose: forall[a] (a, a) -> a"
      , "choose_curry: forall[a] a -> a -> a"
      ]

main ∷ IO()
main = do
    let e = loadEnv env
    -- p ← parse expr ∘ BSC.unpack <$> BSC.getLine
    -- let t = trace (show p) $ infer e ∘ fst ∘ head $ p
    t ← infer e (tok 0) ∘ fst ∘ head ∘ parse expr ∘ BSC.unpack <$> BSC.getLine
    case t of
        Right t → BSC.putStrLn ∘ BSC.pack ∘ show ∘ (\(t, _, _) → t) $ t
        Left s → throwIO $ TypeException s

