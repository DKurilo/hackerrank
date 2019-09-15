{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
{-# OPTIONS_GHC -O2 #-}
-- This exercise has very simple solution in Haskell,
-- because in Haskell we can work with very big Integers
-- But I want to build solution with fair modular arithmetic
-- Also I want to try to use at least Reader Monad somewhere.
-- Maybe it's good idea to create my own monad with modular arithmetic
-- But let it be next time.
module Main where

import Prelude.Unicode -- so no we can use ←, ∷, etc
import Control.Monad.Unicode -- so now we can use ≫, ≫=, etc.
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as DM
import qualified Data.Array as DA
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.Environment
import System.IO

-- inverse x is 1 ≡ (a * x) mod m
-- an = m, x = a(n-1) =>
-- a0 = 1 * k0                 => m0 = 1; d0 = 0
-- a1 = a0 * k1 + 1            => 1 = a1 - a0 * k1 (m1 = d0 - k1; d1 = m0) 1 = a0 * m1 + a1 * d1
-- a2 = a1 * k2 + a0           => a0 = a2 - a1 * k2 => 1 = (a2 - a1 * k2) * m1 + a1 * d1 = 
--                                                     (m2 = d1 - k2 * m1; d2 = m1)
--                                                     1 = a1 * m2 + a2 * d2
-- a3 = a2 * k3 + a1           => a1 = a3 - a2 * k3 => 1 = (a3 - a2 * k3) * m2 + a2 * d2
--                                                     (m3 = d2 - k3 * m2; d3 = m2)
--                                                     1 = a2 * m3 + a3 * d3
-- a4 = a3 * k4 + a2           => a2 = a4 - a3 * k4 => 1 = (a4 - a3 * k4) * m3 + a3 * d3
--                                                     (m4 = d3 - k4 * m3; d4 = m3)
--                                                     1 = a3 * m4 + a4 * d4
-- ...
-- an = a(n-1) * kn + a(n-2)   => a(n-2) = an - a(n-1) * kn =>
--                                       1 = (an - a(n-1) * kn) * m(n-1) + an * d(n-1)
--                                       (mn = d(n-1) - kn * m(n-1); dn = m(n-1)
-- x = if mn < 0 an + mn else mn
exgcd ∷ Int → Int → (Int, Int, Int)
exgcd x 0 = (x, 0, 0)
exgcd 0 x = (x, 0, 0)
exgcd x 1 = (x, 1, 0)
exgcd 1 x = (x, 1, 0)
exgcd x y
    | x ≥ y = let (gd, m, d) = exgcd y y' in (gd, d - k * m, m)
    | otherwise = exgcd y x
    where (k, y') = x `divMod` y

mop ∷ (Monad m) ⇒ (Int → Int → Int) → Int → Int → ReaderT Int m Int
mop f x y = do
    m ← ask
    return $ f x y `mod` m

madd ∷ (Monad m) ⇒ Int → Int → ReaderT Int m Int
madd = mop (+)

msub ∷ (Monad m) ⇒ Int → Int → ReaderT Int m Int
msub = mop (-)

mmul ∷ (Monad m) ⇒ Int → Int → ReaderT Int m Int
mmul = mop (*)

mdiv ∷ (Monad m) ⇒ Int → Int → ReaderT Int m Int
mdiv x y = do
    m ← ask
    let (_, i, _) = exgcd m y
    mmul x (if i < 0 then i + m else i)

mfac ∷ Int → ReaderT Int (State (DM.Map Int Int)) Int
mfac 0 = return 1
mfac x = do
    memo ← get
    case DM.lookup x memo of
        Just y → return y
        _ → do
            y ← mfac (x - 1) ≫= mmul x
            put (DM.insert x y memo)
            return y

calc ∷ (Int, [Int], Int) → ReaderT Int (State (DM.Map Int Int)) Int
calc (t, pcs, ms) = do
    ps ← mfac t
    ps' ← mapM mfac pcs ≫= foldM mmul 1 ≫= mdiv ps
    if ms ≡ 0 then return ps' else mmul ps' ms

calcAll ∷ [(Int, [Int], Int)] → ReaderT Int (State (DM.Map Int Int)) [Int]
calcAll = mapM calc

initialize ∷ Int → [(Int, [Int], Int)] → [Int]
initialize m css = evalState (runReaderT (calcAll css) m) DM.empty

prepare ∷ BSC.ByteString → DA.Array Int (DA.Array Char Int)
prepare bs = DA.listArray (0, BSC.length bs) ∘
                 reverse ∘ BSC.foldl add [DA.array ('a', 'z') [(c,0) | c ← ['a'..'z']]] $ bs
    where add ∷ [DA.Array Char Int] → Char → [DA.Array Char Int]
          add (a:as) c = DA.accum (+) a [(c, 1)]:a:as

getInfo ∷ DA.Array Int (DA.Array Char Int) → Int → Int → (Int, [Int], Int)
getInfo as xs xf = foldl (\(t, pcs, ms) n → let (d, m) = n `divMod` 2 in
                                            (d + t, d:pcs, m+ms)) (0,[],0) $
                         zipWith (-) (i xf) (i (xs - 1))
    where i x = DA.elems $ as DA.! x

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    is ← prepare <$> BSC.getLine
    q ← getInt <$> BSC.getLine
    
    qs ← forM [1..q] $ \_ → do
        (xs:xf:_) ← getInts <$> BSC.getLine
        return $ getInfo is xs xf

    -- BSC.hPutStrLn fptr ∘
    BSC.putStrLn ∘
        BSC.intercalate "\n" ∘ map (BSC.pack ∘ show) ∘ initialize (10^9 + 7) $ qs
    hFlush fptr
    hClose fptr
