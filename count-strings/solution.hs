{-
 - Used docs:
 - https://en.wikipedia.org/wiki/Glushkov%27s_construction_algorithm
 - https://www.cs.odu.edu/~toida/nerzic/390teched/regular/fa/nfa-2-dfa.html
 - http://aix1.uottawa.ca/~jkhoury/markov.htm
 - Parser Monad from Graham Hutton
 - https://en.wikipedia.org/wiki/Modular_arithmetic
 -
 - So:
 - 1. Parse it
 - 2. Construct NFA
 - 3. Convert to DFA
 - 4. Build transition matrix for DFA
 - 5. Multiply matrices using modular arithmetic
 - Done
-}
{-# LANGUAGE OverloadedStrings, UnicodeSyntax, LambdaCase #-}

module Main where

import Prelude.Unicode -- so no we can use ←, ∷, etc
import Control.Monad.Unicode -- so now we can use ≫, ≫=, etc.
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Debug.Trace
import System.Environment
import System.IO
import Data.List (transpose)
import qualified Data.Set as DS
import qualified Data.Map as DM
import qualified Data.ByteString.Char8 as BSC

data ABC = A | B | Empty deriving (Show, Eq, Ord)
data RegExp = St AState | L ABC | Or RegExp RegExp | And RegExp RegExp | Many RegExp
    deriving (Show)
data AState = S ABC Int | SEmpty deriving (Show, Eq, Ord)
type P = [AState]
type D = [AState]
type F = [(AState, AState)]
data NFA = NFA Int P D F deriving (Show)
type DFA = ([(Int, Int)], DM.Map (DS.Set AState) (Int, Bool))
type DFAMatrix = ([Int], MatrixInt)
type Rows = Int
type Cols = Int
type Divisor = Int
data MatrixInt = M Rows Cols [[Int]] deriving (Show)

newtype Parser a = P (String → [(a, String)])

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

abc ∷ Parser ABC
abc = do
    char 'a'
    return A
    <|> do
    char 'b'
    return B

regexp ∷ Parser RegExp
regexp = do
    char '('
    r ← regexp
    regexpPar r
    <|> do
    l ← abc
    char '*'
    return (Many (L l))
    <|> do
    l ← abc
    char '|'
    Or (L l) <$> regexp
    <|> do
    l ← abc
    And (L l) <$> regexp
    <|> do
    L <$> abc

regexpPar ∷ RegExp → Parser RegExp
regexpPar r = do
    string ")*"
    return (Many r)
    <|> do
    string ")|"
    Or r <$> regexp
    <|> do
    string ")"
    And r <$> regexp
    <|> do
    char ')'
    return r

exgcd ∷ Int → Int → (Int, Int, Int)
exgcd x 0 = (x, 0, 0)
exgcd 0 x = (x, 0, 0)
exgcd x 1 = (1, 1, 0)
exgcd 1 x = (1, 1, 0)
exgcd x y
    | x ≥ y = let (gd, m, d) = exgcd y y' in (gd, d - k * m, m)
    | otherwise = exgcd y x
    where (k, y') = x `divMod` y

lcd ∷ Int → Int → Int
lcd x y = x * y `div` gcd
    where (gcd, _, _) = exgcd x y

mop ∷ (Int → Int → Int) → Int → Int → Reader Int Int
mop f x y = do
    m ← ask
    return $ f x y `mod` m

madd ∷ Int → Int → Reader Int Int
madd = mop (+)

msub ∷ Int → Int → Reader Int Int
msub = mop (-)

mmul ∷ Int → Int → Reader Int Int
mmul = mop (*)

mdiv ∷ Int → Int → Reader Int Int
mdiv x y = do
    m ← ask
    let (_, i, _) = exgcd m y
    mmul x (if i < 0 then i + m else i)

mmmul ∷ MatrixInt → MatrixInt → Reader Int MatrixInt
mmmul (M r1 c1 vs1) (M r2 c2 vs2)
    | c1 ≠ r2 = error "Can't multiply these matrix!"
    | otherwise = do
        vs ← forM [0..(r1 - 1)] $ \i →
                forM [0..(c2 - 1)] $ \j → do ms ← forM [0..(c1 - 1)] $ \k →
                                                       mmul (vs1 !! i !! k) (vs2 !! k !! j)
                                             foldM madd 0 ms
        return (M r1 c2 vs)

mmpow ∷ MatrixInt → Int → Reader Int MatrixInt
mmpow m p
    | p ≡ 1 = return m
    | p `mod` 2 ≡ 1 = do
        mp ← mmpow m (p - 1)
        mmmul m mp
    | otherwise = do
        mp ← mmpow m (p `div` 2)
        mmmul mp mp

calc ∷ (DFAMatrix, Int) → Reader Int Int
calc ((es, m), l) = do
    (M _ _ vs) ← mmpow m l
    foldM madd 0 (map (\i → head $ vs !! i) es)

calcAll ∷ [(DFAMatrix, Int)] → Reader Int [Int]
calcAll = mapM calc

count ∷ Int → [(DFAMatrix, Int)] → [Int]
count m ps = runReader (calcAll ps) m

linearize ∷ RegExp → (RegExp, Int)
linearize r = (\(a, s) → (a, s - 1)) $ runState (go r) 1
    where go ∷ RegExp → State Int RegExp
          go (St (S l _)) = go (L l)
          go (L l) = do i ← get
                        modify (+1)
                        return (St (S l i))
          go (Or r1 r2) = do r1' ← go r1
                             r2' ← go r2
                             return (Or r1' r2')
          go (And r1 r2) = do r1' ← go r1
                              r2' ← go r2
                              return (And r1' r2')
          go (Many r) = Many <$> go r

buildNFA ∷ RegExp → NFA
buildNFA r = NFA n (DS.toList (p r')) (DS.toList (d r')) (DS.toList (f r'))
    where (r', n) = linearize r
          l (St _) = False
          l (L _) = error "Unlinearized token is not permitted in l!"
          l (Or r1 r2) = l r1 ∨ l r2
          l (And r1 r2) = l r1 ∧ l r2
          l (Many _) = True
          p (St s) = DS.singleton s
          p (L _) = error "Unlinearized token is not permitted in p!"
          p (Or r1 r2) = DS.union (p r1) (p r2)
          p (And r1 r2) = DS.union (p r1) (if l r1 then p r2 else DS.empty)
          p (Many r1) = p r1
          d (St s) = DS.singleton s
          d (L _) = error "Unlinearized token is not permitted in d!"
          d (Or r1 r2) = DS.union (d r1) (d r2)
          d (And r1 r2) = DS.union (d r2) (if l r2 then d r1 else DS.empty)
          d (Many r1) = d r1
          f (St s) = DS.empty
          f (L _) = error "Unlinearized token is not permitted in d!"
          f (Or r1 r2) = DS.union (f r1) (f r2)
          f (And r1 r2) = DS.union (DS.union (f r1) (f r2))
                                   (DS.fromList [(s1, s2) |
                                                 s1 ← DS.toList (d r1), s2 ← DS.toList (p r2)])
          f (Many r1) = DS.union (f r1)
                                 (DS.fromList [(s1, s2) |
                                               s1 ← DS.toList (d r1), s2 ← DS.toList (p r1)])

nfa2dfa ∷ NFA → DFA
nfa2dfa (NFA n p d f) = runState dfa DM.empty
    where fromP l = DS.fromList ∘ filter (\(S l' _) → l' ≡ l) $ p
          fromF ss l = DS.fromList ∘ map snd ∘
                       filter (\(s', S l' _) → s' `DS.member` ss ∧ l' ≡ l) $ f
          isFinal = or ∘ map (`elem` d) ∘ DS.toList
          dfa = do let ia = fromP A
                   let ib = fromP B
                   put (DM.singleton (DS.singleton SEmpty) (0, False))
                   as ← node 0 ia
                   bs ← node 0 ib
                   return (as ⧺ bs)
          node ∷ Int → DS.Set AState → State (DM.Map (DS.Set AState) (Int, Bool)) [(Int, Int)]
          node i s = if s ≠ DS.empty
                       then do m ← get
                               case s `DM.lookup` m of
                                   Just (k, _) → return [(i,k)]
                                   _ → do let i' = DM.size m
                                          put (DM.insert s (i', isFinal s) m)
                                          let na = fromF s A
                                          let nb = fromF s B
                                          as ← node i' na
                                          bs ← node i' nb
                                          return ((i,i'):(as ⧺ bs))
                       else return []
                    

buildMatrix ∷ DFA → DFAMatrix
buildMatrix (es, sm) = (finals, M n n vs) 
    where n = DM.size sm
          ess = DS.fromList es
          vs = [[if (j,i) `DS.member` ess then 1 else 0 | j ← [0..n]] | i ← [0..n]]
          finals = map fst ∘ filter snd ∘ DM.elems $ sm

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    n ← getInt <$> BSC.getLine

    BSC.intercalate "\n" ∘ map (BSC.pack ∘ show) ∘ count (10^9 + 7) <$> forM [1..n] (\_ → do
            (r:ls:_) ← BSC.words <$> BSC.getLine
            return ( buildMatrix ∘ nfa2dfa ∘ buildNFA ∘ fst ∘ head ∘ parse regexp ∘ BSC.unpack $ r
                   , getInt ls))
        ≫= BSC.putStrLn
    --     ≫= BSC.hPutStrLn fptr
    hFlush fptr
    hClose fptr
