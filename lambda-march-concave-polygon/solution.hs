{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as DL
import Debug.Trace
import System.IO

type Point = (Double, Double)
type Vector = (Double, Double)

data Answer = YES | NO
    deriving (Show, Eq)

isConcave ∷ [Point] → Answer
isConcave vs
    | (length ∘ convexhull ∘ DL.sort $ vs) ≡ length vs = NO
    | otherwise = YES

convexhull ∷ [Point] → [Point]
convexhull [] = []
convexhull (p:ps) = convexhull' p (p:ps) (1,0)

convexhull' :: Point → [Point] → Vector → [Point]
convexhull' fp ((x,y):ps) v
    | np ≡ fp = [fp]
    | otherwise = (np:(convexhull' fp (np:filter (≠np) ps) $ vec np))
    where vec p = (fst p - x, snd p - y)
          np = (\(_,_,p) → p) ∘ maximum ∘ map (\p' → let c = vcos v (vec p') in
                   (if (x,y)≡fp && snd p' < y then 2-c else c,(-(distance (x,y) p')), p')) ∘
               filter (≠(x,y)) $ (fp:ps)

vcos ∷ Vector → Vector → Double
vcos (x1,y1) (x2,y2) = (x1*x2+y1*y2)/(sqrt ((x1*x1+y1*y1)*(x2*x2+y2*y2)))

distance :: Point -> Point -> Double
distance (x1,y1) (x2,y2) = sqrt ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    n ← getInt <$> BSC.getLine

    vs ← (forM [1..n] $ \_ → (\(x:y:_) → (fromIntegral x, fromIntegral y)) <$> getInts <$>
          BSC.getLine) ∷ IO [Point]

    let ans = isConcave vs
    BSC.putStrLn ∘ BSC.pack ∘ show $ ans

