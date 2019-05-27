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

newtype Expression = E [ASPair]
    deriving (Eq, Ord)
instance Show Expression where
    show (E []) = "0"
    show (E ps) = (if o ≡ Add then "" else show o) ⧺ show t ⧺ (join ∘ map show $ ps')
        where (ASP t o:ps') = reverse ps

data ASPair = ASP Term OpAS
    deriving (Eq, Ord)
instance Show ASPair where
    show (ASP t o) = " " ⧺ show o ⧺ " " ⧺ show t

newtype Term = T [MDPair]
    deriving (Eq)
instance Ord Term where
    (T ps1) `compare` (T ps2)
        | fp1 > fp2 = GT
        | fp1 < fp2 = LT
        | l1 > l2 = GT
        | l1 < l2 = LT
        | otherwise = ps1 `compare` ps2
        where fp1 = firstPower ps1
              fp2 = firstPower ps2
              l1 = length ps1
              l2 = length ps2
             
firstPower ∷ [MDPair] → Int
firstPower = go 0
    where go pw [] = pw
          go pw (MDP (F (PN _)) _:ps) = go 0 ps
          go pw (MDP (F PX) Mul:ps) = go 1 ps
          go pw (MDP (FP PX (F (PN n))) Mul:ps) = go n ps
          go _ _ = -1

instance Show Term where
    show (T []) = "1"
    show (T (MDP f Mul:ps)) = if f ≡ F (PN 1)
                                then show (T ps)
                                else show f ⧺ (join ∘ map show $ ps)
    show (T (MDP f Div:ps)) = "1" ⧺ show Div ⧺ show f ⧺ (join ∘ map show $ ps)

data MDPair = MDP Factor OpMD
    deriving (Eq, Ord)
instance Show MDPair where
    show (MDP f@(F PX) Mul) = show f
    show (MDP f@(FP PX _) Mul) = show f
    show (MDP f o) = show o ⧺ show f

data Factor = F Par | FP Par Factor
    deriving (Eq, Ord)
instance Show Factor where
    show (F p) = show p
    show (FP p f) = show p ⧺ "^" ⧺ show f

data Par = PN Int | PX | PU OpAS Term | PP Expression
    deriving (Eq, Ord)
instance Show Par where
    show (PN n) = show n
    show PX = "x"
    show (PU Add t) = show t
    show (PU Sub t) = "-" ⧺ show t
    show (PP e) = "(" ⧺ show e ⧺ ")"

data OpAS = Add | Sub
    deriving (Eq, Ord)
instance Show OpAS where
    show Add = "+"
    show Sub = "-"

data OpMD = Mul | Div
    deriving (Eq, Ord)
instance Show OpMD where
    show Mul = "*"
    show Div = "/"

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

parentheses ∷ Parser Par
parentheses = do
    symbol "("
    e ← expr
    symbol ")"
    return $ PP e

nupar ∷ Parser Par
nupar = parentheses
    <|> do
    symbol "x"
    return PX
    <|> PN <$> number

nufactor ∷ Parser Factor
nufactor = do
    p ← par
    symbol "^"
    FP p <$> nufactor
    <|> F <$> nupar

par ∷ Parser Par
par = parentheses
    <|> do
    o ← opas
    PU o <$> term
    <|> do
    symbol "x"
    return PX
    <|> PN <$> number

factor ∷ Parser Factor
factor = do
    p ← par
    symbol "^"
    FP p <$> factor
    <|> F <$> par

mdpairs ∷ Parser [MDPair]
mdpairs = do
    o ← opmd
    f ← factor
    ps ← mdpairs
    return $ MDP f o:ps
    <|> do
    f ← nufactor
    ps ← mdpairs
    return $ MDP f Mul:ps
    <|>return []

term ∷ Parser Term
term = do
    f ← factor
    ps ← mdpairs
    return ∘ T $ (MDP (F (PN 1)) Mul:MDP f Mul:ps)

aspairs ∷ Parser [ASPair]
aspairs = do
    o ← opas
    t ← term
    ps ← aspairs
    return $ ASP t o:ps
    <|> return []

expr ∷ Parser Expression
expr = do
    t ← term
    ps ← aspairs
    return ∘ E $ (ASP t Add:ps)

simplifyPar ∷ Par → Par
simplifyPar p@(PN _) = p
simplifyPar p@PX = p
simplifyPar (PU o t) = PP $ simplify (E [ASP t o])
simplifyPar (PP e) = PP $ simplify e

simplifyFactor ∷ Factor → Factor
simplifyFactor (F p) = F $ simplifyPar p
simplifyFactor (FP p f) = case p' of
    PN n → case f' of
        F (PN pw) → F (PN (n ^ pw))
        _ → FP p' f'
    _ → FP p' f'
    where p' = simplifyPar p
          f' = simplifyFactor f

multiplyMDPsF ∷ [MDPair] → Factor → [MDPair]
multiplyMDPsF ps (F (PN n))
    | null ps = [MDP (F (PN n)) Mul]
    | otherwise = case head ps of
        MDP (F (PN n')) Mul → MDP (F (PN (n*n'))) Mul : tail ps
        p@(MDP (F (PN n')) Div)
            | n ≥ n' ∧ n `mod` n' ≡ 0 → MDP (F (PN (n `div` n'))) Mul : tail ps
            | n < n' ∧ n' `mod` n ≡ 0 → MDP (F (PN (n' `div` n))) Div : tail ps
            | otherwise → p : MDP (F (PN n)) Mul : tail ps
multiplyMDPsF ps (F PX) = addPower 1 ps
multiplyMDPsF ps (FP PX (F (PN pw))) = addPower pw ps
multiplyMDPsF ps f = sort (MDP f Mul:ps)

divideMDPsF ∷ [MDPair] → Factor → [MDPair]
divideMDPsF ps (F (PN n))
    | null ps = [MDP (F (PN n)) Div]
    | otherwise = case head ps of
        MDP (F (PN n')) Div → MDP (F (PN (n'*n))) Div : tail ps
        p@(MDP (F (PN n')) Mul)
            | n' ≥ n ∧ n' `mod` n ≡ 0 → MDP (F (PN (n' `div` n))) Mul : tail ps
            | n' < n ∧ n `mod` n' ≡ 0 → MDP (F (PN (n `div` n'))) Div : tail ps
            | otherwise → p : MDP (F (PN n)) Div : tail ps
divideMDPsF ps (F PX) = addPower (-1) ps
divideMDPsF ps (FP PX (F (PN pw))) = addPower (-pw) ps
divideMDPsF ps f = sort (MDP f Div:ps)

addPower ∷ Int → [MDPair] → [MDPair]
addPower 0 ps = ps
addPower pw []
    | pw ≡ 1 = [MDP (F PX) Mul]
    | pw > 0 = [MDP (FP PX (F (PN pw))) Mul]
    | otherwise = [MDP (FP PX (F (PN (-pw)))) Div]
addPower pw (MDP (F PX) o : ps)
    | pw' ≡ 1 = MDP (F PX) Mul : ps
    | pw' ≡ 0 = ps
    | pw' > 0 = MDP (FP PX (F (PN pw'))) Mul : ps
    | otherwise = MDP (FP PX (F (PN (-pw')))) Div : ps
    where pw' = pw + (if o ≡ Mul then 1 else (-1)) 
addPower pw (MDP (FP PX (F (PN pw'))) o : ps)
    | pw'' ≡ 1 = MDP (F PX) Mul : ps
    | pw'' ≡ 0 = ps
    | pw'' > 0 = MDP (FP PX (F (PN pw''))) Mul : ps
    | otherwise = MDP (FP PX (F (PN (-pw'')))) Div : ps
    where pw'' = pw + (if o ≡ Mul then pw' else (-pw'))
addPower pw (p:ps) = p:addPower pw ps

multiplyASPsF ∷ [ASPair] → Factor → [ASPair]
multiplyASPsF ps (F (PP (E ps'))) = sort
    [(\(ASP (T ps1) o1) (ASP (T ps2) o2) →
        ASP (T $ foldl (\ps (MDP f o) → case o of
                Mul → multiplyMDPsF ps f
                Div → divideMDPsF ps f) ps1 ps2)
            (if o1 ≡ o2 then Add else Sub)) p1 p2 | p1 ← ps, p2 ← ps']
multiplyASPsF ps f = sort $ map (\(ASP (T ps) o) → ASP (T $ multiplyMDPsF ps f) o) ps

divideASPsF ∷ [ASPair] → Factor → [ASPair]
divideASPsF ps (F (PP (E [ASP (T mps) o]))) =
    map (applyAS o) ∘ foldr (\(MDP f mo) ps → case mo of
        Mul → divideASPsF ps f
        Div → multiplyASPsF ps f) ps $ mps
divideASPsF ps f = map (\(ASP (T ps) o) → ASP (T $ divideMDPsF ps f) o) ps

performMD ∷ [MDPair] → [ASPair]
performMD = foldl (\ps p@(MDP f o) → case o of
        Mul → multiplyASPsF ps ∘ simplifyFactor $ f
        Div → divideASPsF ps ∘ simplifyFactor $ f) [ASP (T [MDP (F (PN 1)) Mul]) Add] ∘ sort

simplifyMDPs ∷ [MDPair] → [MDPair]
simplifyMDPs = sort ∘ map (\(MDP f o) → MDP (simplifyFactor f) o)

simplifyTerm ∷ Term → [ASPair]
simplifyTerm (T ps) = performMD ∘ simplifyMDPs $ ps

applyAS ∷ OpAS → ASPair → ASPair
applyAS o' (ASP t o)
    | o' ≡ o = ASP t Add
    | otherwise = ASP t Sub

simplifyASPs ∷ [ASPair] → [ASPair]
simplifyASPs = sort ∘ join ∘ map (\(ASP t o) → map (applyAS o) ∘ simplifyTerm $ t)

performAS ∷ [ASPair] → [ASPair]
performAS [] = []
performAS [p] = [p]
performAS (ASP (T [MDP (F (PN n1)) Mul]) o1:ASP (T [MDP (F (PN n2)) Mul]) o2:ps)
    | n ≡ 0 = performAS ps
    | n > 0 = performAS (ASP (T [MDP (F (PN n)) Mul]) Add:ps)
    | otherwise = performAS (ASP (T [MDP (F (PN (-n))) Mul]) Sub:ps)
    where n = (if o1 ≡ Add then n1 else (-n1)) + (if o2 ≡ Add then n2 else (-n2))
performAS (ASP (T [MDP (F (PN n1)) Mul, MDP (F PX) Mul]) o1
          :ASP (T [MDP (F (PN n2)) Mul, MDP (F PX) Mul]) o2:ps)
    | n ≡ 0 = performAS ps
    | n > 0 = performAS (ASP (T [MDP (F (PN n)) Mul, MDP (F PX) Mul]) Add:ps)
    | otherwise = performAS (ASP (T [MDP (F (PN (-n))) Mul, MDP (F PX) Mul]) Sub:ps)
    where n = (if o1 ≡ Add then n1 else (-n1)) + (if o2 ≡ Add then n2 else (-n2))
performAS (p1@(ASP (T [MDP (F (PN n1)) Mul, MDP (FP PX pw1) Mul]) o1)
          :p2@(ASP (T [MDP (F (PN n2)) Mul, MDP (FP PX pw2) Mul]) o2):ps)
    | pw1 ≠ pw2 = p1:performAS (p2:ps)
    | n ≡ 0 = performAS ps
    | n > 0 = performAS (ASP (T [MDP (F (PN n)) Mul, MDP (FP PX pw1) Mul]) Add:ps)
    | otherwise = performAS (ASP (T [MDP (F (PN (-n))) Mul, MDP (FP PX pw2) Mul]) Sub:ps)
    where n = (if o1 ≡ Add then n1 else (-n1)) + (if o2 ≡ Add then n2 else (-n2))
performAS (p:ps) = p:performAS ps

simplify ∷ Expression → Expression
simplify e@(E ps)
    | e ≠ e' = simplify e'
    | otherwise = e'
    where e' = E ∘ performAS ∘ simplifyASPs $ ps

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

