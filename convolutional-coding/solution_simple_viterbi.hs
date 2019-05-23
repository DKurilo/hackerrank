{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.List (sort, nub)
import Data.List.Split (chunksOf)
import Data.Array
import Debug.Trace
import System.IO

type Polynom = [Bool]

data Convolution = C Int Int [Polynom]

-- It looks like we have terrible error on HackerRank. So I need to send result inverted..
-- Or even to use wrong character...
newtype Message = M [Bool]
    deriving (Eq)
instance Show Message where
    show (M bs) = map (\b → if b then '0' else '1') bs

emptyR ∷ Int → [Bool]
emptyR n = take n ∘ repeat $ False

readMessage ∷ Message → String
readMessage (M bs) = map (\bs → ['\0'..] !! foldl (\n b → n * 2 + if b then 1 else 0) 0 bs) ∘
                     chunksOf 8 $ bs

applyP ∷ [Polynom] → [Bool] → [Bool]
applyP ps rs = reverse ∘ map (foldl (≠) False ∘ zipWith (&&) rs) $ ps

push ∷ Bool → [Bool] → Int → [Bool]
push b rs n = take n (b:rs)

encode ∷ Convolution → Message → Message
encode (C en ek es) (M bs) = M ∘ reverse ∘ snd ∘
        foldl (\(rs, bs) b → let rs' = push b rs ek in
                   (rs', applyP es rs' ⧺ bs))
              (re, []) $ (bs ⧺ re) 
    where re = emptyR ek

decodeViterbi ∷ Convolution → Message → Message
decodeViterbi (C dn dk ds) (M bs) =
    M [head ∘ x $ i | i ← [tm,tm-1..dk]]
    where nm = 2 ^ dn - 1
          km = 2 ^ dk - 1
          o = (oa !)
          oa = array (0,nm) [(i, i2b dn i) | i ← [0..nm]]
          s = (sa !)
          sa = array (0,km) [(i, i2b dk i) | i ← [0..km]]
          yl = reverse ∘ map reverse ∘ chunksOf dn $ bs
          tm = length yl - 1
          ya = listArray (0, tm) yl
          y = (ya !)
          a i j = aa ! (i,j)
          aa = array ((0,0),(km,km)) [((i,j), a' i j) | j ← [0..km], i ← [0..km]]
          a' i j | push True sj dk ≡ si = 0.5
                 | push False sj dk ≡ si = 0.5
                 | otherwise = 0
               where sj = s j
                     si = s i
          b i j = ba ! (i, j)
          ba = array ((0,0),(tm,km))
                  [((i,j), probability (y i) (applyP ds (s j))) | i ← [0..tm], j ← [0..km]]
          t1 i j = t1a ! (i, j)
          t1a = array ((0,0),(tm,km)) [((i, j), t1' i j) | i ← [0..tm], j ← [0..km]]
          t1' 0 0 = 1
          t1' 0 _ = 0
          t1' i j = let k = head $ t2 i j in
                    cc i * t1 (i - 1) k * a k j * b i j
          cc = (cca !)
          cca = listArray (0,tm) [cc' i | i ← [0..tm]]
          cc' 0 = 1
          cc' i | m ≡ 0 = 1
                | otherwise = (10 ** dnf) / m
              where m = maximum [t1 (i - 1) j | j ← [0..km]]
          t2 0 _ = [0]
          t2 i j = argmax (\k → t1 (i - 1) k * a k j) [0..km]
          z = (za !)
          za = listArray (0, tm) [z' i | i ← [0..tm]]
          z' i | i ≡ tm = nub $ argmax (t1 tm) [0..km]
               | otherwise = nub ∘ join ∘ map (t2 (i + 1)) $ z (i + 1)
          x i | i ≡ tm = s ∘ head ∘ z $ tm
              | otherwise = s ∘ head ∘ z $ i
          dnf = fromIntegral dn
          probability xs ys  =
              (p ** (dnf - d)) * ((1 - p) ** d)
              where d = fromIntegral $ diff xs ys
                    p = 0.7
          connected ∷ Int → Int → Bool
          connected i j | push True si dk ≡ sj = True
                        | push False si dk ≡ sj = True
                        | otherwise = False
               where sj = s j
                     si = s i

argmax ∷ (Ord b) ⇒ (a → b) → [a] → [a]
argmax f as = map fst ∘ foldr (\a b@((am, fam):_) → let fa = f a in
                                          if fa ≡ fam
                                            then (a, fa):b
                                            else if fa > fam
                                                   then [(a, fa)]
                                                   else b) [(head as, f $ head as)] ∘ tail $ as

fac ∷ Double → Double
fac 0 = 1
fac n = product [2..n]

b2i ∷ [Bool] → Int
b2i = foldl (\n b → n * 2 + (if b then 1 else 0)) 0

i2b ∷ Int → Int → [Bool]
i2b n i = fst ∘ foldr (\_ (bs, k) → ((k `mod` 2 ≡ 1):bs, k `div` 2)) ([], i) $ [1..n]

diff ∷ [Bool] → [Bool] → Int
diff bs1 bs2 = foldl (\c (b1,b2) → if b1 ≡ b2 then c else c + 1) 0 $ zip bs1 bs2

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    let getBools = BSC.foldr (\c bs → if c ≡ '0'
                                        then False:bs
                                        else if c ≡ '1'
                                               then True:bs
                                               else bs) []

    (dn: dk: _) ← getInts <$> BSC.getLine
    dp ← forM [1..dn] $ \_ → getBools <$> BSC.getLine

    (en: ek: _) ← getInts <$> BSC.getLine
    ep ← forM [1..en] $ \_ → getBools <$> BSC.getLine

    bs ← getBools <$> BSC.getContents
    let dec = decodeViterbi (C dn dk dp) $ M bs
    BSC.putStrLn ∘ BSC.pack ∘ readMessage $ dec
    -- BSC.putStrLn ∘ BSC.pack ∘ show $ dec
    -- BSC.putStrLn ∘ BSC.pack ∘ show ∘ encode (C en ek ep) ∘ decodeViterbi (C dn dk dp) $ M bs

