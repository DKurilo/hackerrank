{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString.Char8 as BSC
import Data.Array.ST
import Data.Array
import qualified Data.IntSet as DS
import Data.List
import Debug.Trace
import System.IO

data Inmate = C DS.IntSet | L Int -- cell or link
    deriving (Show, Eq)

cost ∷ Int → [[Int]] → Int
cost n ps = foldl addCost 0 ∘ build n $ ps
    where addCost c p = case p of 
              C ns → c + (ceiling ∘ sqrt ∘ fromIntegral $ 1 + DS.size ns)
              otherwise → c

build ∷ Int → [[Int]] → [Inmate]
build n ps = elems $ runSTArray $ do
    let bnd = (1, n)
    ar ← newArray bnd (C DS.empty)
    forM_ ps $ \(p1:p2:_) → if p1 ≡ p2 then return () else do
        cp1 ← readArray ar p1
        cp2 ← readArray ar p2
        case cp1 of
            L t1 → if t1 ≡ p2 then return () else do
                case cp2 of
                    L t2 → if t1 ≡ t2 then return () else do
                        (C ns1) ← readArray ar t1
                        (C ns2) ← readArray ar t2
                        writeArray ar t1 (C $ DS.unions [DS.singleton t2, ns2, ns1])
                        writeArray ar t2 (L t1)
                        forM_ (DS.toList ns2) $ \p → do
                            writeArray ar p (L t1)
                    C ms2 → do
                        (C ns) ← readArray ar t1
                        writeArray ar t1 (C $ DS.unions [DS.singleton p2, ms2, ns])
                        writeArray ar p2 (L t1)
                        forM_ (DS.toList ms2) $ \p → do
                            writeArray ar p (L t1)
            C ms1 → case cp2 of
                L t2 → if p1 ≡ t2 then return () else do
                    (C ns) ← readArray ar t2
                    writeArray ar t2 (C $ DS.unions [DS.singleton p1, ms1, ns])
                    writeArray ar p1 (L t2)
                    forM_ (DS.toList ms1) $ \p → do
                        writeArray ar p (L t2)
                C ms2 → do
                    writeArray ar p1 (C $ DS.unions [DS.singleton p2, ms2, ms1])
                    writeArray ar p2 (L p1)
                    forM_ (DS.toList ms2) $ \p → do
                        writeArray ar p (L p1)
    return ar

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    n ← getInt <$> BSC.getLine
    m ← getInt <$> BSC.getLine
    ps ← forM [1..m] $ \_ → getInts <$> BSC.getLine

    BSC.putStrLn ∘ BSC.pack ∘ show ∘ cost n $ ps

