{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO


data STree = Tip Int Integer | Node STree Int Integer Int STree | Empty
    deriving (Show, Eq)

val ∷ STree → Integer
val (Tip _ v) = v
val (Empty) = 1
val (Node _ _ v _ _) = v

buildT ∷ Int → Int → [Integer] → STree
buildT b _ [] = Empty
buildT b _ [a] = Tip b a
buildT b e as = Node lt b (lcd2 (val lt) (val rt)) e rt
    where m = b + (e - b) `div` 2
          lt = buildT b m $ take (m - b + 1) as
          rt = buildT (m + 1) e $ drop (m - b + 1) as

queryT ∷ STree → (Int, Int) → Integer
queryT Empty (b, e) = 1
queryT (Tip i v) (b, e) = if b ≤ i ∧ e ≥ i then v else 1
queryT (Node lt ib v ie rt) (b, e)
    | ib ≥ b ∧ ie ≤ e = v
    | e < ib ∨ b > ie = 1
    | otherwise = lcd2 (queryT lt (b, e)) (queryT rt (b, e))

updateT ∷ STree → Int → Integer → STree
updateT Empty _ _ = Empty
updateT (Tip i v) j a = if i ≡ j then Tip i $ a * v else Tip i v
updateT (Node lt ib v ie rt) j a
    | j > ie ∨ j < ib = Node lt ib v ie rt
    | otherwise = Node lt' ib (lcd2 (val lt') (val rt')) ie rt'
    where lt' = updateT lt j a
          rt' = updateT rt j a

modulo ∷ Integer
modulo = 10 ^ 9 + 7

gcd2 ∷ Integer → Integer → Integer
gcd2 x 0 = x
gcd2 0 x = x
gcd2 x y
    | x > y = gcd y $ x `mod` y
    | otherwise = gcd x $ y `mod` x

lcd2 :: Integer → Integer → Integer
lcd2 x y
    | d ≡ 1 = x * y
    | otherwise = d * lcd2 (x `div` d) (y `div` d)
    where d = gcd2 x y

lcd ∷ [Integer] → Integer
lcd [] = 0 -- error
lcd (x:[]) = x
lcd (x1:x2:xs) = lcd $ (lcd2 x1 x2):xs

process ∷ ([Integer], STree) → (String, Int, Int) → ([Integer], STree)
process (rs, tr) ("Q", i, j) = (queryT tr (i, j):rs, tr)
process (rs, tr) ("U", i, j) = (rs, (updateT tr i $ fromIntegral j))

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getIntegers = map (fromIntegral ∘ getInt) <$> BSC.split ' '

    n ← getInt <$> BSC.getLine
    tr ← buildT 0 (n - 1) ∘ getIntegers <$> BSC.getLine

    k ← getInt <$> BSC.getLine
    BSC.intercalate "\n" ∘ map (BSC.pack ∘ show ∘ (`mod` modulo)) ∘ reverse ∘ fst <$>
            foldM (\a _ →
                process a ∘ (\(a1:a2:a3:_) → (BSC.unpack a1, getInt a2, getInt a3)) ∘
                BSC.split ' ' <$> BSC.getLine) ([], tr) [1..k] ≫= BSC.putStrLn

