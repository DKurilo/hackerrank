{-# LANGUAGE OverloadedStrings, UnicodeSyntax, LambdaCase #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import Control.Applicative
import Data.Semigroup
import Data.Char
import qualified Data.Map as DM
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

newtype Parser a = P (String → [(a, String)])

newtype Name = N String
    deriving (Eq, Ord)
instance Show Name where
    show (N s) = s

newtype Var = Var Name
    deriving (Eq, Ord)
instance Show Var where
    show (Var n) = '#':show n

innerV ∷ Var → Bool
innerV (Var (N n)) = head n ≡ '_'

data Term = V Var | R RTerm
    deriving (Eq, Ord)
instance Show Term where
    show (V v) = show v
    show (R r) = show r

data RTerm = RT Name [Term]
    deriving (Eq, Ord)
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

data Res = Unsat | Sat (DM.Map Var Term)
    deriving (Eq, Ord)
instance Show Res where
    show Unsat = "UNSAT"
    show (Sat vs)
        | vs' ≡ DM.empty = "SAT"
        | otherwise = "SAT:\n=====" ⧺
                    foldl (\cs (v, t) → cs ⧺ "\n" ⧺ show v ⧺
                                        " := " ⧺ show t) "" (DM.assocs vs')
        where vs' = DM.filterWithKey (\k a → not ∘ innerV $ k) vs

newtype Results = RS [Res]
    deriving (Eq, Ord)
instance Show Results where
    show (RS rs) = foldl (\cs r → if cs ≡ "UNSAT"
                                    then show r
                                    else cs ⧺ "\n" ⧺ show r) "UNSAT" rs

unsat ∷ Results
unsat = RS [Unsat]

instance Semigroup Results where
    (<>) r1@(RS rs1) r2@(RS rs2)
        | r1 ≡ unsat ∨ r2 ≡ unsat = unsat
        | otherwise = RS $ rs1 ⧺ rs2

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
    xs ← many (sat (\c → isAlphaNum c ∨ c ≡ '-'))
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

newtype Tokenizer = Tok (Tokenizer, [Op])

tokenize ∷ Int → [Op] → (Tokenizer, [Op])
tokenize c os = (Tok next, os')
    where (c', os') =  foldr (\o (c, os) → let (c', o') = tokenizeL c o in
                                           (c', o':os)) (c, []) os
          next = tokenize c' os

tokenizeL ∷ Int → Op → (Int, Op)
tokenizeL c (Rule cts t) = (c + DM.size m', Rule ucts t')
    where (t', m) = go t DM.empty
          (ucts, m') = foldr (\ct (cts', m') → case ct of
                   T t' → let (ut, m'') = go t' m' in (T ut:cts', m'')
                   C cn t1 t2 → let (ut1, m'') = go t1 m'
                                    (ut2, m''') = go t2 m'' in
                                (C cn ut1 ut2:cts', m''')) ([], m) cts
          go ∷ Term → DM.Map Var Var → (Term, DM.Map Var Var)
          go (V (Var (N n))) m = case DM.lookup (Var $ N n) m of
                  Just v → (V v, m)
                  _ → (V $ Var $ N un, DM.insert (Var $ N n) (Var $ N un) m)
              where un = '_': show (c + DM.size m)
          go (R (RT n ts)) m = (R $ RT n ts', m')
              where (ts', m') = foldr (\t (ts', m') → let (t', m'') = go t m' in
                                                      (t':ts',m'')) ([], m) ts
applyMap ∷ DM.Map Var Term → [CTerm] → [CTerm]
applyMap m [] = []
applyMap m (T t:cts) = (T $ applyMapT m t):applyMap m cts
applyMap m (C c t1 t2:cts) = C c (applyMapT m t1) (applyMapT m t2):applyMap m cts

applyMapT ∷ DM.Map Var Term → Term → Term
applyMapT m (V v) = case DM.lookup v m of
        Just t → t
        _ → V v
applyMapT m (R (RT n ts)) = R (RT n $ map (applyMapT m) ts)

compareT ∷ DM.Map Var Term → Term → Term → Maybe (DM.Map Var Term)
compareT m (V v1) (V v2) 
    | v1 ≡ v2 = Just m
    | otherwise = let mt1 = DM.lookup v1 m
                      mt2 = DM.lookup v2 m in
                  case mt1 of
                      Just t1 | mt1 ≡ mt2 → Just m
                              | mt2 ≡ Nothing → Just $ selfUpdate $ DM.insert v2 t1 m
                              | otherwise → Nothing
                      _ | mt1 ≡ mt2 → Just $ selfUpdate $ DM.insert v1 (V v2) m
                        | otherwise → Just $ selfUpdate $ DM.insert v1 ((\(Just v) → v) mt2) m
compareT m (V v) t@(R _) = case DM.lookup v m of
    Just t' | t ≡ t' → Just m
            | otherwise → Nothing
    _ → Just $ selfUpdate $ DM.insert v t m
compareT m t@(R _) (V v) = case DM.lookup v m of
    Just t' | t ≡ t' → Just m
            | otherwise → Nothing
    _ → Just $ selfUpdate $ DM.insert v t m
compareT m t1@(R (RT n1 ts1)) t2@(R (RT n2 ts2))
    | n1 ≡ n2 ∧ length ts1 ≡ length ts2 = foldl (\mm (t1,t2) →
                case mm of
                    Just m' → compareT m' (applyMapT m' t1) (applyMapT m' t2)
                    _ → Nothing) (Just m) $ zip ts1 ts2
    | otherwise = Nothing

selfUpdate ∷ DM.Map Var Term → DM.Map Var Term
selfUpdate m = DM.map (applyMapT m) m

addMap ∷ DM.Map Var Term → Results → Results
addMap m (RS rs) = RS $ map (\case
                                  Unsat → Unsat
                                  Sat m' → Sat $ selfUpdate $ DM.union m' m) rs

checkT ∷ DM.Map Var Term → Tokenizer → CTerm → (Tokenizer, Results)
checkT m (Tok (tkn, os)) (T t) = (tkn', rs)
    where t' = applyMapT m t
          (tkn', rs) = foldr (\(Rule rcts rt) (tkn, rs) → case compareT m t' rt of
                  Just m' → let (tkn', rs') = prove tkn (Query $ applyMap m' rcts) in
                            (tkn', rs <> addMap m' rs')
                  _ → (tkn, rs)) (tkn, RS []) os

checkC ∷ DM.Map Var Term → CTerm → Res
checkC m (C cnd t1 t2) = case compareT m (applyMapT m t1) (applyMapT m t2) of
    Just m' | cnd ≡ Eq → Sat m'
            | otherwise → Unsat
    _ | cnd ≡ Ne → Sat m
      | otherwise → Unsat

proveCT ∷ (Tokenizer, Results) → CTerm → (Tokenizer, Results)
proveCT (t, RS rs) ct@T{} = (t', rss')
    where (t', rss') = foldr (\r (t, rss) → case r of
                                              Unsat → (t, rss)
                                              Sat m → let (t', rss') = checkT m t ct in
                                                      (t', rss' <> rss)) (t, RS []) rs
proveCT (t, RS rs) ct@C{} = (t, RS $ foldr (\(Sat m) rs → let r = checkC m ct in
                                                          if r ≡ Unsat
                                                            then rs
                                                            else r:rs) [] rs)

prove ∷ Tokenizer → Op → (Tokenizer, Results)
prove t (Query []) = (t, RS [Sat DM.empty])
prove t (Query cts) = (t', rs)
    where (t', rs) = foldl proveCT (t, RS [Sat DM.empty]) cts
prove t _ = (t, RS [])

-- it's not the best way to handle not equal conditions.
-- It can be time and memory conuming. But sorry, it' easiest way with my current solution.
-- Next time I need to think about it earlier and to keep separately assignments and conditions.
-- Or I can even replace DM.Map with some other data structure that will work with conditions
moveNeCond ∷ Op → Op
moveNeCond (Query cts) = Query $ uncurry (⧺) ∘ foldr (\ct (cts,nects) → case ct of
        C Ne _ _ → (cts,ct:nects)
        _ → (ct:cts,nects)) ([],[]) $ cts

main ∷ IO()
main = takeWhile (≠QUIT) ∘ map fst ∘ join ∘ map (parse op ∘ BSC.unpack) ∘ BSC.split '\n' <$>
       BSC.getContents ≫=
       foldM_ (\os o → case o of
                   Rule _ _ → do
                       BSC.putStrLn "Ok." --  ∘ BSC.pack ∘ show $ o
                       return (o:os)
                   Query _ → do
                       let (_, r) = prove (Tok $ tokenize 0 os) $ moveNeCond o
                       BSC.putStrLn ∘ BSC.pack ∘ show $ r
                       BSC.putStrLn "Ready."
                       return os
                   QUIT → do
                       BSC.putStrLn "Bye."
                       return os
                   _ → return os) [] ∘ (⧺ [QUIT])

