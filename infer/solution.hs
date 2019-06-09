{-# LANGUAGE UnicodeSyntax, LambdaCase #-}
module Main where
{--
 -
 - Damas-Hindley-Milner Algorithm J implementation
 - https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system
 - https://en.wikipedia.org/wiki/Unification_(computer_science)
 -
 -}

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import Control.Exception
import Control.Applicative
import Data.Either
import Data.Char
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.List (intercalate, (\\))
import Debug.Trace
import System.IO

newtype Parser a = P (String → [(a, String)])

type Ident = String

data Expr = EL Ident Expr Expr | EF [Ident] Expr | ES SExpr | EIT Ident Type
    deriving (Eq, Ord, Show)

data SExpr = SEP Expr | SEI Ident  | SEFC SExpr [Expr]
    deriving (Eq, Ord, Show)

data Type = Any | TU [Type] Type | TFA [Ident] Type Substitution | TC SType Type | TST SType
    deriving (Eq, Ord, Show)

data SType = STP Type | STI Ident | STG SType [Type]
    deriving (Eq, Ord, Show)

type Substitution = DM.Map Ident Type
type Instantiation = DM.Map Ident Type
type Context = DM.Map Ident Type

newtype TypeException = TypeException String

instance Exception TypeException

instance Show TypeException where
    show (TypeException s) = "Type error: " ⧺ s

data Token = Tkn Token Ident

instance Show Token where
    show (Tkn n t) = show t

data State = S { stkn ∷ Token
               , sctx ∷  Context
               , ssubst ∷ Substitution
               , sinst ∷ Instantiation
               }
    deriving (Show)

tkn ∷ Int → Token
tkn c = Tkn tkn' v
    where v = 'α':show c
          tkn' = tkn (c+1)

prettytkn ∷ Int → Token
prettytkn c = Tkn tkn' v
    where tl = map (:[]) ['a'..'z'] ⧺ zipWith (\i n → i: show n) (cycle ['a'..'z']) [0..]
          v = tl !! c
          tkn' = prettytkn (c+1)

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
    e ← expr
    symbol "in "
    EL i e <$> expr
    <|> do
    symbol "fun "
    as ← argList
    symbol "-> "
    EF as <$> expr
    <|> do
    i ← ident
    symbol ":"
    EIT i <$> typep
    <|> ES <$> sexpr

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
    t ← typep
    return $ TFA as t DM.empty
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

addCtx ∷ State → Ident → Type → Either String State
addCtx (S tk ctx subst inst) i τ = Right $ S tk (DM.insert i τ ctx) subst inst

removeCtx ∷ State → Ident → Either String State
removeCtx (S tk ctx subst inst) i = Right $ S tk (DM.delete i ctx) subst inst

addSubst ∷ State → Ident → Type → Either String State
addSubst (S tk ctx subst inst) i τ = Right $ S tk ctx (DM.insert i τ subst) inst

addInst ∷ State → Ident → Type → Either String State
addInst (S tk ctx subst inst) i τ = Right $ S tk ctx subst (DM.insert i τ inst)

newvar ∷ State → Either String (Type, State)
newvar (S (Tkn tkn i) ctx subst inst) =
    Right (TST (STI i), S tkn ctx subst (DM.insert i Any inst))

instantiate ∷ State → Ident → Either String (Type, State)
instantiate st i = case DM.lookup i (ssubst st) of
    Just Any → Right (TST (STI i), st)
    Just τ@(TST (STI i')) → instantiate st i'
    Just t → Right (simplifyType t, st)
    _ → if or ∘ map (compareI st i) $ ses
          then Right (TST (STI i), st)
          else case DM.lookup i (sinst st) of
                   Just Any → Right (TST (STI i), st)
                   Just (TST (STI i')) → instantiate st i'
                   Just t → let (Right (t', st')) = instantiateT st t in
                            Right (simplifyType t', st')
                   _ → Right (TST (STI i), st)
    where subst = ssubst st
          ses = DM.elems ∘ DM.map (\(TST (STI i)) → i) $ subst

instantiateT ∷ State → Type → Either String (Type, State)
instantiateT st (TU τs τ) = Right (TU τs' τ', st'')
    where (Right (τs', st')) = instantiateTs st τs
          (Right (τ', st'')) = instantiateT st' τ
instantiateT st (TC sτ τ) = Right (TC sτ' τ', st'')
    where (Right (sτ', st')) = instantiateST st sτ
          (Right (τ', st'')) = instantiateT st' τ
instantiateT st (TST sτ) = Right (simplifyType $ TST sτ', st')
    where (Right (sτ', st')) = instantiateST st sτ

instantiateTs ∷ State → [Type] → Either String ([Type], State)
instantiateTs st = foldr (\τ (Right (τs, st))  → let (Right (τ', st')) = instantiateT st τ in
                                                 Right (τ':τs, st')) (Right ([], st))

instantiateST ∷ State → SType → Either String (SType, State)
instantiateST st (STP τ) = Right (simplifySType $ STP τ', st')
    where (Right (τ', st')) = instantiateT st τ
instantiateST st (STI i) = Right (simplifySType (STP τ'), st')
    where (Right (τ', st')) = instantiate st i
instantiateST st (STG sτ τs) = Right (STG sτ' τs', st'')
    where (Right (sτ', st')) = instantiateST st sτ
          (Right (τs', st'')) = instantiateTs st' τs

generalize' ∷ State → Type → (Type → Instantiation → Ident → Bool) → Either String (Type, State)
generalize' st τ p = Right (TFA (DS.elems is) τ' subst, st')
    where inst = sinst st'
          upi = DM.elems ∘ DM.map (\(TST (STI i)) → i) ∘ ssubst $ st'
          ids = DS.filter (`notElem` upi) ∘ identsT $ τ'
          (Right (τ', st')) = instantiateT st τ
          is = DS.filter (free (p τ) inst) ids
          nf = ids DS.\\ is
          subst = DM.filterWithKey (\k v → k `DS.member` nf) inst

finalGeneralize ∷ State → Type → Either String (Type, State)
finalGeneralize st τ = generalize' st τ (\_ _ _ → True)

generalize ∷ State → Type → Either String (Type, State)
generalize st τ = 
    generalize' st τ
        (\τ inst i → not ∘ or ∘ map (\τ → i `DS.member` identsT τ) ∘ DM.elems $ inst)

free ∷ (Instantiation → Ident → Bool) → Instantiation → Ident → Bool
free p inst i = case DM.lookup i inst of
   Just Any → p inst i
   _ → False

identsT ∷ Type → DS.Set Ident
identsT Any = DS.empty
identsT (TU τs τ) = identsTs (τ:τs)
identsT (TFA is τ _) = DS.union (DS.fromList is) (identsT τ)
identsT (TC sτ τ) = DS.union (identsST sτ) (identsT τ)
identsT (TST sτ) = identsST sτ

identsST ∷ SType → DS.Set Ident
identsST (STP τ) = identsT τ
identsST (STI i) = DS.singleton i
identsST (STG sτ τs) = DS.union (identsST sτ) (identsTs τs)

identsTs ∷ [Type] → DS.Set Ident
identsTs = foldl (\s τ → DS.union s (identsT τ)) DS.empty

tokenize ∷ State → Type → Either String (Type, State)
tokenize st Any = Right (Any, st)
tokenize st (TU τs τ) = case r1 of
    Right _ → case r2 of
        Right _ → Right (simplifyType $ TU τs' τ', st'')
    Left e → Left e
    where r1 = tokenizes st τs
          (Right (τs', st')) = r1
          r2 = tokenize st' τ
          (Right (τ', st'')) = r2
tokenize st (TC sτ τ) = case r1 of
    Right _ → case r2 of
        Right _ → Right (simplifyType $ TC sτ' τ', st'')
    Left e → Left e
    where r1 = tokenizeS st sτ
          (Right (sτ', st')) = r1
          r2 = tokenize st' τ
          (Right (τ', st'')) = r2
tokenize st (TST sτ) = case r1 of
    Right _ → Right  (simplifyType $ TST sτ', st')
    where r1 = tokenizeS st sτ
          (Right (sτ', st')) = r1
tokenize st (TFA is τ subst) = Right (τ', S tk ctx (ssubst st) inst)
    where st' = foldl (\st i → let (Right (τ, st')) = newvar st
                                   (Right st'') = addSubst st' i τ in st'') st is
          (S tk' ctx' subst' inst', inst'') =
              DM.foldrWithKey (\i τ (st, mi) → let (Right (τ', st')) = tokenize st τ in
                                               (st', DM.insert i τ' mi)) (st', DM.empty) subst
          st'' = S tk' ctx' subst' (DM.union inst'' inst')
          (Right (τ', S tk ctx _ inst)) = tokenize st'' τ

tokenizeS ∷ State → SType → Either String (SType, State)
tokenizeS st (STP τ) = case r of
    Right _ → Right (simplifySType $ STP τ', st')
    Left e → Left e
    where r = tokenize st τ
          (Right (τ', st')) = r
tokenizeS st sτ@(STI i) = case DM.lookup i (ssubst st) of
    Just τ → Right (simplifySType $ STP τ, st)
    _ → Right (sτ, st)
tokenizeS st (STG sτ τs) = case r1 of
    Right _ → case r2 of
        Right _ → Right (simplifySType $ STG sτ' τs', st'')
        Left e → Left e
    Left e → Left e
    where r1 = tokenizeS st sτ
          (Right (sτ', st')) = r1
          r2 = tokenizes st' τs
          (Right (τs', st'')) = r2

tokenizes ∷ State → [Type] → Either String ([Type], State)
tokenizes st = foldr (\τ r → case r of
    Right (τs, st) → let r1 = tokenize st τ in
                     case r1 of
                         Right (τ', st') → Right (τ':τs, st')
                         Left e → Left e
    Left e → Left e) (Right ([], st))

simplifyType ∷ Type → Type
simplifyType Any = Any
simplifyType (TU τs τ) = TU (map simplifyType τs) (simplifyType τ)
simplifyType (TFA is τ subst) = TFA is (simplifyType τ) subst
simplifyType (TC sτ τ) = TC (simplifySType sτ) (simplifyType τ)
simplifyType (TST (STP τ)) = simplifyType τ
simplifyType (TST sτ) = TST $ simplifySType sτ

simplifySType ∷ SType → SType
simplifySType (STP (TST (STP τ))) = STP τ
simplifySType (STP (TST (STI i))) = STI i
simplifySType (STP τ) = STP $ simplifyType τ
simplifySType sτ@(STI _) = sτ
simplifySType (STG sτ τs) = STG (simplifySType sτ) (map simplifyType τs)

unify ∷ State → Type → Type → Either String State
unify st τ1 τ2 = unify' st (simplifyType τ1) (simplifyType τ2)

unify' ∷ State → Type → Type → Either String State
unify' st τ1@(TST (STI i)) τ2
    | τ1 ≡ τ2 = Right st
    | otherwise = case DM.lookup i (sinst st) of
            Just Any | occursCheck st i τ2 → addInst st i τ2
                     | otherwise → Left $ "Recursion found while checking " ⧺ i
                                        ⧺ " in " ⧺ show τ2
            Just τ1' → unify st τ1' τ2
            _ → unify st τ2 τ1
unify' st τ1 τ2@(TST (STI i)) = case DM.lookup i (sinst st) of
    Just Any | occursCheck st i τ1 → addInst st i τ1
             | otherwise → Left $ "Recursion found while checking " ⧺ i ⧺ " in " ⧺ show τ1
    Just τ2' → unify st τ1 τ2'
unify' st (TU τs1 τ1) (TU τs2 τ2) = case unifys st τs1 τs2 of
            Right st' →  unify st' τ1 τ2
            Left e → Left e
unify' st (TC sτ1 τ1) (TC sτ2 τ2) = case unifyST st sτ1 sτ2 of
            Right st' →  unify st' τ1 τ2
            Left e → Left e
unify' st (TST sτ1) (TST sτ2) = unifyST st sτ1 sτ2
unify' st τ1 τ2
    | τ1 ≡ τ2 = Right st
    | otherwise = Left $ "Can't unify " ⧺ show τ1 ⧺ " and " ⧺ show τ2

unifys ∷ State → [Type] → [Type] → Either String State
unifys st τs1 τs2 
    | length τs1 ≡ length τs2 = case r of
            Right st' → Right st'
            Left e → Left e
    | otherwise = Left "Can' unify types with different argument's lenght"
    where r = foldl (\r (τ1, τ2) → case r of
                        Right st → unify st τ1 τ2
                        Left e → Left e) (Right st) $ zip τs1 τs2

unifyST ∷ State → SType → SType → Either String State
unifyST st (STG sτ1 τs1) (STG sτ2 τs2) = case unifyST st sτ1 sτ2 of
    Right st' → unifys st' τs1 τs2
    Left e → Left e
unifyST st sτ1 sτ2 = unify st (st2t sτ1) (st2t sτ2)

st2t ∷ SType → Type
st2t (STP τ) = τ
st2t sτ = TST sτ

find ∷ State → Type → Either String Type
find st (TST (STI i)) = case DM.lookup i (sinst st) of
    Just τ → Right τ
    _ → Left $ "Found unbounded variable " ⧺ show i
find st τ = Right τ

occursCheck ∷ State → Ident → Type → Bool
occursCheck st i (TST (STI i'))
    | i ≡ i' = False
    | otherwise = case DM.lookup i' (sinst st) of
            Just τ → occursCheck st i τ
            _ → True
occursCheck st i Any = True
occursCheck st i (TU τs τ) = and ∘ map (occursCheck st i) $ τ:τs
occursCheck st i (TC sτ τ) = occursCheck st i τ ∧ occursCheck st i (TST sτ)
occursCheck st i (TST (STP τ)) = occursCheck st i τ
occursCheck st i (TST (STG sτ τs)) = and ∘ map (occursCheck st i) $ TST sτ:τs

compose ∷ State → State → State
compose (S otk octx osubst oinst) (S ntk nctx nsubst ninst) = S tk ctx subst inst
    where tk = ntk
          ctx = DM.union nctx octx
          subst = osubst
          inst = DM.union ninst oinst

compareI ∷ State → Ident → Ident → Bool
compareI st i1 i2 = build i1 ≡ build i2
    where inst = sinst st
          build i = case DM.lookup i inst of
                        Just (TST (STI i')) → build i'
                        _ → i

infer ∷ State → Expr → Either String (Type, State)
-- [Let]
infer st (EL i e0 e1) = case r1 of
    Right _ → case r2 of
        Right _ → case r3 of
            Right _ → Right (τ', st''''')
            Left e → Left e
        Left e → Left e
    Left e → Left e
    where r1 = infer st e0
          (Right (τ, st')) = r1
          r2 = generalize st' τ
          (Right (γτ, st'')) = r2
          (Right st''') = addCtx st'' i γτ
          r3 = infer st''' e1
          (Right (τ', st'''')) = r3
          (Right st''''') = removeCtx (st `compose` st'''') i
-- [Abs]
infer st (EF [i] e) = case r1 of
    Right _ → case r2 of
        Right _ → Right (TC (STP τ) τ', st `compose` st'')
        Left e → Left e
    Left e → Left e
    where r1 = addVar st i
          (Right (τ, st')) = r1
          r2 = infer st' e
          (Right (τ', st'')) = r2
infer st (EF is e) = case r1 of
    Right _ → case r2 of
        Right _ → Right (TU τs τ', st `compose` st'')
        Left e → Left e
    Left e → Left e
    where r1 = foldr (\i r → case r of
                  Right (τs, st) → case addVar st i of
                      Right (τ, st') → Right (τ:τs, st')
                      Left e → Left e) (Right ([], st)) is
          (Right (τs, st')) = r1
          r2 = infer st' e
          (Right (τ', st'')) = r2

infer st (ES se) = case inferS st se of
    Right (τ, st') → Right (τ, st `compose` st')
    Left e → Left e

infer (S tkn ctx subst inst) (EIT i t) = Right (t, S tkn (DM.insert i t ctx) subst inst)

addVar ∷ State → Ident → Either String (Type, State)
addVar st i = Right (τ, st''')
    where (Right (τ, st')) = newvar st
          (Right st'') = addSubst st' i τ
          (Right st''') = removeCtx st'' i

inferS ∷ State → SExpr → Either String (Type, State)
inferS st (SEP e) = case infer st e of
    Right (τ, st') → Right (τ, st `compose` st')
    Left e → Left e
-- [Var]
inferS st (SEI i) = case DM.lookup i (sctx st) of
    Just t → tokenize st t
    _ → instantiate st i
-- [App]
inferS st (SEFC se es) = case r1 of
    Right _ → case r2 of
        Right _ → case r3 of
            Right _ → case r4 of
                Right _ → Right (τ', st `compose` st'''')
                Left e → Left e
            Left e → Left e
        Left e → Left e
    Left e → Left e
    where r1 = inferS st se
          (Right (τ0, st')) = r1
          r2 = foldr (\e r → case r of
                  Right (τs, st) → let r1 = infer st e in
                                    case r1 of
                                        Right (τ, st') → Right (τ:τs, st `compose` st')
                                        Left e → Left e
                  Left e → Left e) (Right ([], st')) es
          (Right (τs, st'')) = r2
          r3 = newvar st''
          (Right (τ', st''')) = r3
          r4 = if length τs == 1
                 then unify st''' τ0 (TC (simplifySType ∘ STP ∘ head $ τs) τ')
                 else unify st''' τ0 (TU τs τ')
          (Right st'''') = r4

loadEnv ∷ [String] → Context
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



prettyPrint ∷ Type → String
prettyPrint = pp DM.empty (prettytkn 0)

pp ∷ DM.Map Ident Ident → Token → Type → String
pp s t Any = "oops" -- error
pp s t (TU τs τ) = "(" ⧺ ppTs s t τs ⧺ ") -> " ⧺ pp s t τ
pp s t (TFA [] τ _) = pp s t τ
pp s t (TFA is τ _) = "forall[" ⧺ (unwords ∘ reverse $ is') ⧺ "] " ⧺ pp s' t τ
    where (is', s', t') = foldl (\(is, s, Tkn tn tc) i → let s' = DM.insert i tc s in
                                                         (tc:is, s', tn)) ([], s, t) $
                          uncurry (⧺) ∘ srt is $ τ
pp s t (TC sτ τ) = ppS s t sτ ⧺ " -> " ⧺ pp s t τ
pp s t (TST sτ) = ppS s t sτ

ppTs ∷ DM.Map Ident Ident → Token → [Type] → String
ppTs s t = intercalate ", " ∘ map (pp s t)

ppS ∷ DM.Map Ident Ident → Token → SType → String
ppS s t (STP τ) = "(" ⧺ pp s t τ ⧺ ")"
ppS s t (STI i) = case DM.lookup i s of
    Just i' → i'
    _ → i
ppS s t (STG sτ τs) = ppS s t sτ ⧺ "[" ⧺ ppTs s t τs ⧺ "]"

srt ∷ [Ident] → Type → ([Ident], [Ident])
srt [] _ = ([], [])
srt is Any = ([], is)
srt is (TU τs τ) = (is' ⧺ is'', ris')
    where (is', ris) = srtTs is τs
          (is'', ris') = srt ris τ
srt is (TFA is' τ _) = (is'', ris')
    where dis = is \\ is'
          (is'', ris) = srt dis τ
          ris' = is \\ dis
srt is (TC sτ τ) = (is' ⧺ is'', ris')
    where (is', ris) = srtS is sτ
          (is'', ris') = srt ris τ
srt is (TST sτ) = srtS is sτ

srtS ∷ [Ident] → SType → ([Ident], [Ident])
srtS is (STP τ) = srt is τ
srtS is (STI i) = if i `elem` is then ([i], is \\ [i]) else ([], is)
srtS is (STG sτ τs) = (is' ⧺ is'', ris')
    where (is', ris) = srtS is sτ
          (is'', ris') = srtTs ris τs

srtTs ∷ [Ident] → [Type] → ([Ident], [Ident])
srtTs is = foldl (\(is, ris) τ → let (is', ris') = srt ris τ in
                                 (is ⧺ is', ris')) ([],is)

main ∷ IO()
main = do
    let e = loadEnv env
    t ← infer (S (tkn 0) (loadEnv env) DM.empty DM.empty) ∘ fst ∘ head ∘ parse expr <$> getLine
    case t of
        Right (t, st) → case finalGeneralize st t of
            Right (τ, st') → putStrLn ∘ prettyPrint $ τ
            Left s → throwIO $ TypeException s
        Left s → throwIO $ TypeException s

