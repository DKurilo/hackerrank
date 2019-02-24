{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import GHC.Arr
import Data.Array
import qualified Data.Array.Base as A
import qualified Data.Array.Unboxed as U
import Data.Array.ST
import Data.Int
import Data.Bits
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import qualified Data.Map.Strict as DM
import Data.Bits

-- Complete the decibinaryNumbers function below.
{- I found!
 - Check image https://kurilo.us/20190222_132421.jpg
 - and this article: http://jelv.is/blog/Lazy-Dynamic-Programming/  -}
mn ∷ Int64
mn = 142556 -- 285112 -- 300000

md ∷ Int64
md = 19 -- digits ∘ dec2bin $ mn

-- to compile with profiler:
-- ghc -prof -fprof-auto -rtsopts ./solution.hs
-- to run with profiler:
-- ./solution +RTS -p < in3
-- profile in solution.prof

decibinaryNumbers ∷ Int64 → Int64
-- decibinaryNumbers x = decibinaryNumbers'' x 0
-- decibinaryNumbers x = decibinaryNumbers' x [0] 1
decibinaryNumbers w = DL.foldl' (+) 0 ∘ map snd ∘ takeWhileInclusive (\(k,_) → k > 0) ∘ 
                      map (\i -> let k = kr i in 
                                 if k ≡ 0 then (k, nw i) else (k, 10^k)) $ [0..]
    where cs = genCs
          ccs = genCCs
          rccs = genRCCs
          c ∷ Int64 → Int64 → Int64
          c d n
              | d < -1 ∧ n < 0 = 0
              | d ≡ -1 ∧ n ≡ 0 = 1
              | d ≡ -1 = 0
              | otherwise = A.unsafeAt cs ∘ unsafeIndex bcs $ (d,n `shift` (-1))
          dec = case DM.lookupGE w ccs of
              Just (_,x) -> x
              Nothing -> 0
          mj = if dec ≡ 0 then 1 else 1 + 9 * ((digits ∘ dec2bin $ dec) - 1)
          nw ∷ Int64 → Int64
          nw = (A.unsafeAt nws ∘ unsafeIndex bnws)
          nw' ∷ Int64 → Int64
          -- nw' 0 = snd ∘ head ∘ dropWhile (\(σ,_) → σ < w) ∘ tail ∘
          --         scanl (\(σ,_) i → (c md i + σ,i)) (0,0) $ [0..]
          nw' 0 = dec
          nw' n = nw (n-1) - (pow2 ∘ kr $ n-1)
          nws ∷ Array Int64 Int64
          nws = listArray bnws $ map (\x -> if x < 0 then 0 else x) [nw' j | j ← range bnws]
          bnws = (0,mj)
          r ∷ Int64 → Int64
          r = (A.unsafeAt rs ∘ unsafeIndex brs)
          r' ∷ Int64 → Int64
          r' 0 = case rccs DM.!? (dec - 1) of
                     Just x ->  w - x
                     Nothing -> w
          -- DL.foldl' (\σ i → σ + (c md i)) 0 [0..(nw 0 - 1)]
          r' n = (r (n-1)) - (c ((kr $ n-1) - 1) (nw $ n-1))
          rs ∷ Array Int64 Int64
          rs = listArray brs [r' j | j ← range brs]
          brs = (0,mj)
          kr ∷ Int64 → Int64
          kr = (A.unsafeAt krs ∘ unsafeIndex bkrs)
          kr' ∷ Int64 → Int64
          kr' n = get 0
          --         snd ∘ head ∘ dropWhile (\(σ,_) → σ < rn) ∘
          --         map (\i → (c i nwn,i)) $ [0..]
              where nwn = nw n
                    rn = r n
                    get ∷ Int64 → Int64
                    get i | c i nwn ≥ rn = i
                          | otherwise = get (i+1)
          krs ∷ Array Int64 Int64
          krs = listArray bkrs [kr' j | j ← range bkrs]
          bkrs = (0,mj)

takeWhileInclusive ∷ (a → Bool) → [a] → [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

-- compiler told me it was slow. Sorry.
pow2 ∷ Int64 -> Int64
pow2 n = 1 `shift` (fromIntegral n)

bcs ∷ ((Int64,Int64),(Int64,Int64))
bcs = ((0,0), (md, mn))

genCs ∷ U.UArray (Int64, Int64) Int64
genCs = runSTUArray $ do
    let bd = [0,1,2,3,4,5,6,7,8,9]
    cs ← newArray bcs 0
    let go d n = do
                 cc ← foldM (\σ j → do
                                      let n'=n-j*(pow2 d)
                                      if d ≡ 0 ∧ n' ≡ 0
                                      then return $ σ + 1
                                      else if n' < 0 ∨ d ≡ 0
                                      then return σ
                                      else do
                                             cp ← A.unsafeRead cs ∘ unsafeIndex bcs $ 
                                                  (d - 1, n' `shift` (-1))
                                             return $ σ + cp
                             ) 0 bd
                 A.unsafeWrite cs (unsafeIndex bcs (d, n `shift` (-1))) cc
                 let (d', n') = if d ≡ md
                                then (0, n+2)
                                else (d+1,n)
                 if n' ≡ mn * 2 + 2
                 then return ()
                 else do
                     go' ← go d' n'
                     return $ go'
    go 0 0
    return cs

genCCs :: DM.Map Int64 Int64
genCCs = DM.fromList ∘ tail ∘ 
         scanl (\(ccs,_) i → (ccs + genCs U.! (md,i `shift` (-1)),i)) (0,0) $ [0..(mn*2)]

genRCCs :: DM.Map Int64 Int64
genRCCs = DM.fromList ∘ tail ∘ 
          scanl (\(_,ccs) i → (i,ccs + genCs U.! (md,i `shift` (-1)))) (0,0) $ [0..(mn*2)]
-- It was great idea... but...
{-
c ∷ Int64 → Int64 → Int64
c d n
    | d < -1 ∧ n < 0 = 0
    | d ≡ -1 ∧ n ≡ 0 = 1
    | d ≡ -1 = 0
    | otherwise = cs ! (d,n)
c' ∷ Int64 → Int64 → Int64
c' d n = foldr (\j σ → let n' = n-j*2^d in
                       if n' < 0 then 0
                       else σ + c (d-1) n') 0 [0..9]
cs ∷ Array (Int64,Int64) Int64
cs = listArray bcs [c' i j | (i,j) ← range bcs]
bcs = ((0,0), (md, mn))
-}

dec2bin ∷ Int64 → Int64
dec2bin 0 = 0
dec2bin x = 10 * dec2bin d + m
    where (d,m) = divMod x 2

digits ∷ Int64 → Int64
digits 0 = 0
digits x = 1 + (digits $ x `div` 10)

-- Rest of functions just to remember what I did to make it woorking
{-
decibinaryNumbers'' :: Int64 -> Int64 -> Int64
decibinaryNumbers'' x n
    | sz >= x = DS.elemAt (fromIntegral x - 1) s
    | otherwise = decibinaryNumbers'' (x - sz) $ n + 1
    where s = db2dec n
          sz = fromIntegral . DS.size $ s

decibinaryNumbers' :: Int64 -> [Int64] -> Int64 -> Int64
decibinaryNumbers' x ns p
    | p >= x = ns !! (fromIntegral x - 1)
    | otherwise = decibinaryNumbers' (x - p) ns' p'
    where (ns', p') = getNextNumbers ns

getNextNumbers :: [Int64] -> ([Int64], Int64)
getNextNumbers [] = ([], 0)
getNextNumbers (x:xs) = (xs' ++ xs'', (fromIntegral.length $ xs') + p'')
    where (xs'', p'') = getNextNumbers xs
          xs' = getNext $ x+1

getNext :: Int64 -> [Int64]
getNext x
    | x `mod` 10 == 0 = []
    | x `mod` 10 == 2 = x:(map (*10) . getNext $ 1 + x `div` 10)
    | otherwise = [x]

testIdea :: Int64 -> Int64
testIdea x = mx - mn + 1
    where mx = floor $ (log $ fromIntegral x) / log 2 
          mn = floor $ 1 + (((log $ fromIntegral x) - log 10) / log 2)

testIdea1 :: Int64 -> Int64
testIdea1 x = foldr (\n a -> a + foldr (addend n) 0 [0..9]) 0 $ [0..(digits.dec2bin $ x)]
    where addend n y b = b + if (x - y * 2 ^ n > 0) then x - y * 2 ^ n else 0

dec2db :: Int64 -> Int64
dec2db = dec2db' 0

dec2db' :: Int64 -> Int64 -> Int64
dec2db' _ 0 = 0
dec2db' p n = m * 2 ^ p + dec2db' (p+1) d
    where (d,m) = divMod n 10

db2dec :: Int64 -> DS.Set Int64
db2dec x = db2dec' (dec2bin x) DS.empty

db2dec' :: Int64 -> DS.Set Int64 -> DS.Set Int64
db2dec' x s
    | DS.member x s = s
    | otherwise = DS.insert x $ DL.foldr db2dec' s $ reldb 1 $ x

reldb :: Int64 -> Int64 -> [Int64]
reldb n x
    | n > digits x = []
    | otherwise = if d' >= 10 then reldb (n+1) x
                  else if d == 0 then reldb (n+1) x
                  else x':reldb (n+1) x
    where p = 10 ^ n
          p1 = 10 ^ (n - 1)
          d = x `div` p `mod` 10
          d' = x `div` p1 `mod` 10 + 2
          x' = x `div` (10 * p) * 10 * p + (d - 1) * p + 2 * p1 + x `mod` p
-}
main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    q <- readLn :: IO Int

    forM_ [1..q] $ \q_itr -> do
        x <- readLn :: IO Int64

        let result = decibinaryNumbers x

        -- hPutStrLn fptr $ show result
        putStrLn $ show result

    hFlush fptr
    hClose fptr

