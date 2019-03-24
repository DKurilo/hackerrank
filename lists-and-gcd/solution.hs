{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.Environment
import System.IO

data Factor = F Int Int -- prime, power
    deriving (Eq)
instance Show Factor where
    show (F n p) = show n ⧺ " " ⧺ show p

factors ∷ [Int] → [Factor]
factors [] = []
factors (_:[]) = []
factors (n:p:fs) = (F n p): factors fs

flgcd ∷ [[Factor]] → [Factor]
flgcd [] = []
flgcd (fs:[]) = fs
flgcd (fs:lfs) = fgcd fs $ flgcd lfs

fgcd ∷ [Factor] → [Factor] → [Factor]
fgcd _ [] = []
fgcd [] _ = []
fgcd ((F n1 p1):fs1) ((F n2 p2):fs2)
    | n1 ≡ n2 = (F n1 $ min p1 p2):fgcd fs1 fs2
    | n1 > n2 = fgcd ((F n1 p1):fs1) fs2
    | n1 < n2 = fgcd fs1 ((F n2 p2):fs2)

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    q ← getInt <$> BSC.getLine
    lfs ← forM [1..q] $ \_ → factors ∘ getInts <$> BSC.getLine

    BSC.putStrLn ∘ BSC.intercalate " " ∘ map (BSC.pack ∘ show) ∘ flgcd $ lfs

