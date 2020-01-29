{-|
  Here is examples from these lectures:
  https://www.youtube.com/watch?v=PAqzQMzsUU8
  https://www.youtube.com/watch?v=jpl7FE2TZTE
  Also a great article is here:
  https://www.schoolofhaskell.com/user/bartosz/understanding-algebras
  I just wrote down examples from lectures as accurate as I could.
  As I feel it, playing with this code allow to understand lectures better and to find why and where you need this.
  This code available here:
  https://gist.github.com/DKurilo/5e5563f4c2a8e8ca53a7b98ebf59e9f1
  You can also play with it on Repl.it
  https://repl.it/@DimaKurilo/Catamorphisms
-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import qualified Data.Map  as M
import           Prelude   hiding (div)
import           System.IO

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coa = Fix . fmap (ana coa) . coa

-- Example 1. Fibonacci
data NatF a = ZeroF | SuccF a deriving (Functor)

type Nat = Fix NatF

intToNat :: Integer -> Nat
intToNat 0 = Fix ZeroF
intToNat n = Fix . SuccF . intToNat $ (n-1)

fib :: Algebra NatF (Integer, Integer)
fib ZeroF          = (0, 1)
fib (SuccF (m, n)) = (n, m + n)

-- Example 2. Sum
data ListF e a = NulF | ConsF e a deriving (Functor)

type List a = Fix (ListF a)

listToList :: [a] -> List a
listToList []     = Fix NulF
listToList (x:xs) = Fix . ConsF x $ listToList xs

sumAlg :: Algebra (ListF Int) Int
sumAlg NulF        = 0
sumAlg (ConsF e a) = e + a

-- Example 3. Primes
data StreamF e a = StreamF e a deriving (Functor)

era :: Coalgebra (StreamF Int) [Int]
era (p:ns) = StreamF p (filter (nodiv p) ns)

nodiv p x = x `mod` p /= 0

primes :: Fix (StreamF Int)
primes = ana era [2..]

streamToList :: Fix (StreamF e) -> [e]
streamToList (Fix (StreamF e a)) = e:streamToList a

-- Example 4. Expression
data ExprF a = PlusF a a
             | TimesF a a
             | DivF a a
             | PowF a a
             | NegF a
             | ConstF Double
             | VarF String
    deriving (Functor)

type Expr = Fix ExprF

eval :: M.Map String Double -> Algebra ExprF Double
eval _ (PlusF x y) = x + y
eval _ (TimesF x y) = x * y
eval _ (DivF x y) = x / y
eval _ (PowF x y) = x ** y
eval _ (NegF x) = - x
eval _ (ConstF x) = x
eval vs (VarF v) = case v `M.lookup` vs of
                      Just x -> x
                      _ -> error $ "Variable " ++ v ++ " not found in map!"

add :: Expr -> Expr -> Expr
add x y = Fix (PlusF x y)

mul :: Expr -> Expr -> Expr
mul x y = Fix (TimesF x y)

div :: Expr -> Expr -> Expr
div x y = Fix (DivF x y)

pow :: Expr -> Expr -> Expr
pow x y = Fix (PowF x y)

neg :: Expr -> Expr
neg x = Fix (NegF x)

num :: Double -> Expr
num x = Fix (ConstF x)

var :: String -> Expr
var v = Fix (VarF v)

main = do
    print . fst . cata fib . intToNat $ 10000
    print . cata sumAlg . listToList $ [1..100000]
    print (streamToList primes !! 1000)
    let myEval = eval (M.fromList [("x", 42 :: Double), ("y", 23 :: Double)])
        myExpr = add (mul (num 5) (pow (var "x") (num 2)))
                     (neg (mul (add (mul (num 6) (var "y")) (mul (num 4) (var "x")))
                               (add (var "x") (neg (pow (var "y") (div (num 1) (num 3))))))) -- 5x² - (6y + 4x)(x - y¹ᐟ³)
    print . cata myEval $ myExpr
