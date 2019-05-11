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
        where vs' = vs -- DM.filterWithKey (\k a → not ∘ innerV $ k) vs

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
        | r1 ≡ unsat ∧ r2 ≡ unsat = unsat
        | null rs1 ∧ r2 ≡ unsat = unsat
        | null rs2 ∧ r1 ≡ unsat = unsat
        | r1 ≡ unsat = unsat
        | r2 ≡ unsat = unsat
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
                              | otherwise → trace (show (V v1, V v2, m)) Nothing
                      _ | mt1 ≡ mt2 → Just $ selfUpdate $ DM.insert v1 (V v2) m
                        | otherwise → Just $ selfUpdate $ DM.insert v1 ((\(Just v) → v) mt2) m
compareT m (V v) t@(R _) = case DM.lookup v m of
    Just t' | t ≡ t' → Just m
            | otherwise → trace (show (V v, t, m)) Nothing
    _ → Just $ selfUpdate $ DM.insert v t m
compareT m t@(R _) (V v) = case DM.lookup v m of
    Just t' | t ≡ t' → Just m
            | otherwise → trace (show (t, V v, m)) Nothing
    _ → Just $ selfUpdate $ DM.insert v t m
compareT m t1@(R (RT n1 ts1)) t2@(R (RT n2 ts2))
    | n1 ≡ n2 ∧ length ts1 ≡ length ts2 =
        let q = foldl (\mm (t1,t2) →
                case mm of
                    Just m' → compareT m' (applyMapT m' t1) (applyMapT m' t2)
                    _ → Nothing) (Just m) $ zip ts1 ts2 in
                        case q of
                            Just m' → trace ("Just " ++ show (t1, t2, m')) q
                            _  → trace (show (t1, t2, m)) Nothing
    | otherwise = Nothing

selfUpdate ∷ DM.Map Var Term → DM.Map Var Term
selfUpdate m = DM.map (applyMapT m) m

check ∷ Int → [Op] → [CTerm] → Term → [CTerm] → (Results, Int)
check c _ _ _ [] = (RS [Sat DM.empty], c)
check c os rcts rt (T t:ts) = case compareT DM.empty rt t of
    Just m → let m' = selfUpdate m
                 (os', c') = tokenizeA c os
                 (RS rs, c'') = prove c' os' (Query $ applyMap m' rcts)
                 (os'', c''') = tokenizeA c'' os
                 rs' = trace ("1 @@" ++ show rs ++ "@@") $ map (\case
                                  Unsat → Unsat
                                  Sat m'' → Sat (selfUpdate $ DM.union m'' m')) rs in
                 if RS rs ≠ unsat
                   then foldl (\(rrs, rc) (Sat m'') →
                            let (os', rc') = tokenizeA rc os
                                (rrs', rc'') = prove rc' os' (Query $ applyMap m'' ts) in
                            trace ("2 ##" ++ show rrs' ++ "##") $ (rrs <>
                              (\(RS rrs'') → trace ("3 %%" ++ show rrs'' ++ "%%") $ RS $
                                  map (\case
                                           Unsat → Unsat
                                           Sat m' → Sat (selfUpdate $ DM.union m m'')) rrs'')
                                rrs', rc'')) (RS [], c''') (trace ("4 $$" ++ show rs' ++ "$$") rs')
                   else (unsat, c'')
    _ → (RS [], c)
check c os rcts rt (C cn t1 t2:cts) = trace (show (t1, t2)) $ case compareT DM.empty t1 t2 of
    Just _ | cn ≡ Eq → check c os rcts rt cts
           | otherwise → (unsat, c)
    _ | cn ≡ Ne → check c os rcts rt cts
      | otherwise → (unsat, c)

prove ∷ Int → [Op] → Op → (Results, Int)
prove c os (Query []) = (RS [Sat DM.empty], c)
prove c os (Query cts) = foldl (\(rs, c') (Rule rcts rt) → 
                                    let (rs', c'') = check c os rcts rt cts in
                                    (rs <> rs', c'')) (RS [], c) os
prove c _ _ = (RS [], c)

tokenizeA ∷ Int → [Op] → ([Op], Int)
tokenizeA c = foldr (\o (os', c') → let (o', c'') = tokenize c' o in
                                      (o':os', c'')) ([], c)

tokenize ∷ Int → Op → (Op, Int)
tokenize c (Rule cts t) = (Rule ucts t', c + DM.size m')
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

main ∷ IO()
main = takeWhile (≠QUIT) ∘ map fst ∘ join ∘ map (parse op ∘ BSC.unpack) ∘ BSC.split '\n' <$>
       BSC.getContents ≫=
       foldM_ (\(os, c) o → case o of
                   Rule _ _ → do
                       let (o', c') = tokenize c o
                       BSC.putStrLn ∘ BSC.pack ∘ show $ o'
                       BSC.putStrLn "Ok." --  ∘ BSC.pack ∘ show $ o
                       return (o':os, c')
                   Query _ → do
                       let (r, c') = prove c (reverse os) o
                       BSC.putStrLn ∘ BSC.pack ∘ show $ r
                       BSC.putStrLn "Ready."
                       return (os, c')
                   QUIT → do
                       BSC.putStrLn "Bye."
                       return (os, c)
                   _ → return (os, c)) ([], 0) ∘ (⧺ [QUIT])

