{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Control.Monad.ST
import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Array.ST as DAS
import qualified Data.Array as DA
import Debug.Trace

data Node = N Int [Int]
    deriving (Show, Eq)

cost ∷ Int
cost = 6

distances ∷ [(Int,Int)] → Int → Int → [Int]
distances es n s = filter (≠0) ∘ map (\(N d _) → d) ∘ DA.elems $ 
                   DAS.runSTArray $ djikstra [s] 0 cost ∘ buildG n $ es

djikstra ∷ [Int] → Int → Int → ST s (DAS.STArray s Int Node) → ST s (DAS.STArray s Int Node)
djikstra ss d c g = do
    g' ← g
    ss' ←  forM ss $ \s' → do
        (N cd ns) ← DAS.readArray g' s'
        if cd ≡ (-1) then do
            DAS.writeArray g' s' (N (d * c) ns)
            return ns
        else return []
    let ss'' = join ss'
    if ss'' ≡ [] then return g'
    else djikstra (join ss') (d+1) c $ return g'

buildG ∷ Int → [(Int,Int)] → ST s (DAS.STArray s Int Node)
buildG n es = do
    g ← DAS.newArray (1,n) (N (-1) []) ∷ ST s (DAS.STArray s Int Node)
    forM_ es $ \(u,v) → do
        (N _ ns1) ← DAS.readArray g u
        DAS.writeArray g u (N (-1) (v:ns1)) 
        (N _ ns2) ← DAS.readArray g v
        DAS.writeArray g v (N (-1) (u:ns2))
    return g

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 1
    let getPair = map getInt <$> BSC.split ' ' <$> BSC.getLine
    q ← getInt <$> BSC.getLine
    forM_ [1..q] $ \_ → do
        (n:m:_) ← getPair 
        es ← forM [1..m] $ \_ → do
            (u:v:_) ← getPair
            return (u,v)
        s ← getInt <$> BSC.getLine
        let ds = distances es n s
        BSC.putStrLn ∘ BSC.intercalate " " ∘ map (BSC.pack ∘ show) $ ds

