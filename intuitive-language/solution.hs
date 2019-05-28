{-# LANGUAGE UnicodeSyntax, LambdaCase #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import Control.Applicative
import Data.Char
import Data.Ratio
import Data.List (intercalate)
import qualified Data.Map as DM
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

newtype Parser a = P (String → [(a, String)])

type Program = [Line]

data Line = Fact Var Function | Do Expr Line | Assign [(Var,Expr)] | What [Expr] | Comment String
    deriving (Show, Eq)

data Function = Fun [Expr] Expr
    deriving (Eq)

instance Show Function where
    show (Fun [] exp) = show ∘ simplify $ exp
    show (Fun exps exp) = (intercalate ", " ∘ map (show ∘ simplify) $ exps) ⧺ ", " ⧺ show exp

numb2expr ∷ Numb → Expr
numb2expr n = Exp [ASP Add (T [MDP Mul (FN n)])]

numb2fun ∷ Numb → Function
numb2fun n = Fun [] $ numb2expr n

tfun2numb ∷ Function → Numb
tfun2numb (Fun _ (Exp [ASP Add (T [MDP Mul (FN n)])])) = n
tfun2numb (Fun _ (Exp [ASP Sub (T [MDP Mul (FN (N r))])])) = N (-r)
tfun2numb (Fun _ (Exp [ASP Add (T [MDP Mul (FU Sub (FN (N r)))])])) = N (-r)

addFuns ∷ Function → Function → Function
addFuns f1@(Fun eks ek0) f2@(Fun _ ek0') = Fun eks ∘ numb2expr $ n
    where n = (\(N r1) (N r2) → N $ r1 + r2) (tfun2numb f1) (tfun2numb f2)

subFuns ∷ Function → Function → Function
subFuns f1@(Fun eks ek0) f2@(Fun _ ek0') = Fun eks ∘ numb2expr $ n
    where n = (\(N r1) (N r2) → N $ r2 - r1) (tfun2numb f1) (tfun2numb f2)

mulFuns ∷ Function → Function → Function
mulFuns f1@(Fun eks ek0) f2@(Fun _ ek0') = Fun eks ∘ numb2expr $ n
    where n = (\(N r1) (N r2) → N $ r1 * r2) (tfun2numb f1) (tfun2numb f2)

divFuns ∷ Function → Function → Function
divFuns f1@(Fun eks ek0) f2@(Fun _ ek0') = Fun eks ∘ numb2expr $ n
    where n = (\(N r1) (N r2) → N $ r2 / r1) (tfun2numb f1) (tfun2numb f2)

type Counter = Int

type Var = String

newtype Numb = N (Ratio Int)
    deriving (Eq, Ord)

instance Show Numb where
    show (N r)
        | q ≡ 1 = show p
        | otherwise = show p ⧺ "/" ⧺ show q
        where p = numerator r
              q = denominator r

data OpAS = Add | Sub
    deriving (Eq)

instance Show OpAS where
    show Add = "+"
    show Sub = "-"

data OpMD = Mul | Div
    deriving (Eq)

instance Show OpMD where
    show Mul = "*"
    show Div = "/"

newtype Expr = Exp [ASPair]
    deriving (Eq)

instance Show Expr where
    show (Exp []) = "0"
    show (Exp (ASP Add t:as)) = show t ⧺ (join ∘ map show $ as)
    show (Exp (ASP Sub t:as)) = show Sub ⧺ show t ⧺ (join ∘ map show $ as)

data ASPair = ASP OpAS Term
    deriving (Eq)

instance Show ASPair where
    show (ASP o t) = " " ⧺ show o ⧺ " " ⧺ show t

newtype Term = T [MDPair]
    deriving (Eq)

instance Show Term where
    show (T []) = "1"
    show (T (MDP Mul f:as)) = show f ⧺ (join ∘ map show $ as)
    show (T (MDP Div f:as)) = "1" ⧺ show Div ⧺ show f ⧺ (join ∘ map show $ as)

data MDPair = MDP OpMD Factor
    deriving (Eq)

instance Show MDPair where
    show (MDP o f) = " " ⧺ show o ⧺ " " ⧺ show f

data Factor = FV Var [Expr] | FN Numb | FU OpAS Factor | FP Expr
    deriving (Eq)

instance Show Factor where
    show (FV vn _) = vn
    show (FN n) = show n
    show (FU Add f) = show f
    show (FU Sub f) = show Sub ⧺ show f
    show (FP e) = "(" ⧺ show e ⧺ ")"

type State = DM.Map String Function

simplifyFactor ∷ Factor → Numb
simplifyFactor (FV _ _) = N (0 % 1) -- error
simplifyFactor (FN n) = n
simplifyFactor (FU Add f) = simplifyFactor f
simplifyFactor (FU Sub f) = let (N r) = simplifyFactor f in N (-r)
simplifyFactor (FP e) = simplify e

simplifyTerm ∷ Term → Numb
simplifyTerm (T ps) = N $ foldl (\r (MDP o f) → let (N r') = simplifyFactor f in case o of
    Mul → r * r'
    Div → r / r') (1 % 1) ps

simplify ∷ Expr → Numb
simplify (Exp ts) = N $ foldl (\r (ASP o t) → let (N r') = simplifyTerm t in case o of
    Add → r + r'
    Sub → r - r') (0 % 1) ts

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
    char (toLower x)
    string xs
    return (x:xs)
    <|> do
    char (toUpper x)
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

arguments ∷ Parser [Expr]
arguments = do
    symbol "["
    a ← expr
    symbol "]"
    as ← arguments
    return $ a:as
    <|> return []

factor ∷ Parser Factor
factor = FN <$> numb
    <|> do
    v ← var
    FV v <$> arguments
    <|> do
    o ← opas
    FU o <$> factor
    <|> do
    symbol "("
    e ← expr
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
    return ∘ T $ MDP Mul f : ps

aspairs ∷ Parser [ASPair]
aspairs = do
    o ← opas
    t ← term
    ps ← aspairs
    return $ ASP o t : ps
    <|> return []

expr ∷ Parser Expr
expr = do
    t ← term
    ps ← aspairs
    return ∘ Exp $ ASP Add t : ps

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

counter ∷ Parser Counter
counter = do
    space
    ns ← some digit
    return $ read ns

numb ∷ Parser Numb
numb = do
    space
    ps ← some digit
    return ∘ N $ read ps % 1

var ∷ Parser Var
var = do
    space
    n ← sat isAlpha
    ns ← many (sat isAlphaNum)
    return $ n:ns

assignment ∷ Parser (Var, Expr)
assignment = do
    e ← expr
    symbol "to"
    v ← var
    return (v,e)

assignments ∷ Parser [(Var, Expr)]
assignments = do
    a ← assignment
    symbol "AND"
    as ← assignments
    return $ a:as
    <|> do
    a ← assignment
    return [a]
    <|> return []

exprs ∷ Parser [Expr]
exprs = do
    e ← expr
    symbol "AND"
    es ← exprs
    return $ e:es
    <|> do
    e ← expr
    return [e]
    <|> return []

line ∷ Parser Line
line = do
    symbol "#"
    s ← many $ sat (≠'\n')
    char '\n'
    return $ Comment s
    <|> do
    vn ← var
    symbol "is"
    symbol "function of"
    c ← counter
    symbol ":"
    ks ← forM [1..c] $ \_ → do
                              k ← expr
                              symbol ","
                              return k
    k0 ← expr
    symbol "."
    return $ Fact vn (Fun ks k0)
    <|> do
    vn ← var
    symbol "is"
    k0 ← expr
    symbol "."
    return $ Fact vn (Fun [] k0)
    <|> do
    symbol "do"
    symbol "{"
    c ← expr
    symbol "}"
    Do c <$> line
    <|> do
    symbol "assign"
    as ← assignments
    symbol "!"
    return $ Assign as
    <|> do
    symbol "what is"
    es ← exprs
    symbol "?"
    return $ What es

program ∷ Parser Program
program = do
    l ← line
    ls ← program
    return $ l:ls
    <|> return []

evalFunction ∷ State → Var → [Expr] → Function
evalFunction s vn eas = Fun (drop (length eas) eks) $ numb2expr (N $ k0 + sum (zipWith (*) ks as))
    where (Fun eks ek0) = s DM.! vn
          eval = map (\e → let (N r) = tfun2numb ∘ evalExpr s $ e in r)
          ks = eval eks
          as = eval eas
          (N k0) = tfun2numb ∘ evalExpr s $ ek0

evalFactor ∷ State → Factor → Function
evalFactor s (FV vn as) = evalFunction s vn as
evalFactor s (FN n) = numb2fun n
evalFactor s (FU Add f) = evalFactor s f
evalFactor s (FU Sub f) = numb2fun ∘ (\(N r) → N (-r)) ∘ tfun2numb ∘ evalFactor s $ f
evalFactor s (FP exp) = evalExpr s exp

evalTerm ∷ State → Term → Function
evalTerm s (T ps) =
    foldl (\fn (MDP o f) → let fn' = evalFactor s f in case o of
        Mul → mulFuns fn' fn
        Div → divFuns fn' fn) (numb2fun (N $ 1 % 1)) ps

evalExpr ∷ State → Expr → Function
evalExpr s (Exp ps) =
    foldl (\fn (ASP o t) → let fn' = evalTerm s t in case o of
        Add → addFuns fn' fn
        Sub → subFuns fn' fn) (numb2fun (N $ 0 % 1)) ps

evalLine ∷ State → Line → (State, [Function])
evalLine s (Fact v f) = (DM.insert v f s, [])
evalLine s (Do e l) = foldl (\(s, nss) _ → let (s', nss') = evalLine s l in
                                           (s', nss' ⧺ nss)) (s, []) [1..n]
    where (N r) = tfun2numb ∘ evalExpr s $ e
          n = numerator r `div` denominator r
evalLine s (Assign ves) =
    (foldl (\s (v, e) → DM.insert v (evalExpr s e) s) s ves, [])
evalLine s (What exps) = (s, map (evalExpr s) exps)
evalLine s (Comment _) = (s, [])

eval ∷ Program → [Function]
eval = snd ∘ foldl (\(s,r) l → let (s', r') = evalLine s l in
                                         (s', r ⧺ r')) (DM.empty, [])

main ∷ IO()
-- main = BSC.pack ∘ show ∘ parse program ∘ BSC.unpack <$> BSC.getContents ≫= BSC.putStrLn
main = BSC.pack ∘ intercalate "\n" ∘ map show ∘ eval ∘ fst ∘ head ∘
       parse program ∘ BSC.unpack <$> BSC.getContents ≫= BSC.putStrLn

