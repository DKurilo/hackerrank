{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.List.Split (chunksOf)
import Debug.Trace
import System.IO

type Polynom = [Bool]

data Convolution = C Int Int [Polynom]

newtype Message = M [Bool]
    deriving (Eq)
instance Show Message where
    show (M bs) = map (\b → if b then '1' else '0') bs

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
    where re = take ek ∘ repeat $ False

decode ∷ Convolution → Message → Message
decode (C dn dk ds) mes@(M bs) = M ∘ take (length bs `div` dn - dk) ∘ reverse ∘ snd ∘
        foldl guess (re, []) ∘ map reverse ∘ chunksOf dn $ bs
    where re = take dk ∘ repeat $ False
          guess ∷ ([Bool], [Bool]) → [Bool] → ([Bool], [Bool])
          guess (rs, bs) ms = (push b' rs dk, b':bs)
              where next b = applyP ds $ push b rs dk
                    s0 = next False
                    s1 = next True
                    b' = findClosest ms s0 s1

findClosest ∷ [Bool] → [Bool] → [Bool] → Bool
findClosest bs s0 s1 = uncurry (>) ∘
        foldl (\(c0, c1) (b, b0, b1) → (c0 + diff b b0, c1 + diff b b1)) (0, 0) $ zip3 bs s0 s1
    where diff b0 b1 = if b0 ≡ b1 then 0 else 1

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
    BSC.putStrLn ∘ BSC.pack ∘ readMessage ∘ decode (C dn dk dp) $ M bs
    BSC.putStrLn ∘ BSC.pack ∘ show ∘ encode (C en ek ep) ∘ decode (C dn dk dp) $ M bs

