{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import qualified Data.Set as DS
import System.IO

modulo ∷ Int
modulo = 101

data Op = Oa | Os | Om
    deriving (Eq)

instance Show Op where
    show Oa = "+"
    show Os = "-"
    show Om = "*"

nextOp ∷ Op → Op
nextOp Oa = Os
nextOp Os = Om
nextOp Om = Oa

data Expr = Ex Expr Op Int | Numb Int

instance Show Expr where
    show (Numb n) = show n
    show (Ex e o n) = show e ⧺ show o ⧺ show n

eval ∷ Expr → Int
eval (Numb n) = n
eval (Ex e o n)
    | o ≡ Oa = (eval e + n) `mod` modulo
    | o ≡ Os = (eval e - n) `mod` modulo
    | o ≡ Om = (eval e * n) `mod` modulo

type Cache = DS.Set (Int, Int)

find ∷ [Int] → Expr
find [] = Numb 0 -- error
find (n:ns) = case (fst $ find' ns (Numb n) DS.empty 0) of
    Just e → e
    _ → Numb 0

find' ∷ [Int] → Expr → Cache → Int → (Maybe Expr, Cache)
find' [] e c l
    | eval e ≡ 0 = (Just e, c)
    | otherwise = (Nothing, c)
find' (n:ns) e c l
    | eval e ≡ 0 = find' ns (Ex e (if n ≡ 0 then Oa else Om) n) c (l + 1)
    | otherwise = case mea of
        Just ea' → (mea, ca)
        _ → case mes of
             Just es → (mes, cs)
             _ → memcm
    where ea = Ex e Oa n
          ca' = DS.insert (eval ea, l) c
          (mea, ca) = if DS.member (eval ea, l) c
                        then (Nothing, c)
                        else find' ns ea ca' (l + 1)
          es = Ex e Os n
          cs' = DS.insert (eval es, l) ca
          (mes, cs) = if DS.member (eval es, l) ca
                        then (Nothing, ca)
                        else find' ns es cs' (l + 1)
          em = Ex e Om n
          cm' = DS.insert (eval em, l) cs
          memcm = if DS.member (eval em, l) cs
                    then (Nothing, cs)
                    else find' ns em cm' (l + 1)

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    n ← getInt <$> BSC.getLine
    BSC.pack ∘ show ∘ find ∘ getInts <$> BSC.getLine ≫= BSC.putStrLn

