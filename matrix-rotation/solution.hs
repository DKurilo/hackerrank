{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.Array
import Debug.Trace
import System.IO

rotate ∷ Array (Int, Int) a → Int → Array (Int, Int) a
rotate mx r = array bnd [(i, mx ! (rindex i r bnd)) | i ← range bnd]
    where bnd = bounds mx

rindex ∷ (Int, Int) → Int → ((Int, Int), (Int, Int)) → (Int, Int)
rindex (i, j) r ((i0, j0), (im, jm)) = moveCell ((i0 + ci, j0 + ci),(im - ci, jm - ci)) (i, j) s
    where ci = minimum [i - i0, j - j0, im - i, jm - j]
          cl = (im + jm - i0 - j0 - ci * 4) * 2
          s = r `mod` cl

moveCell ∷ ((Int, Int), (Int, Int)) → (Int, Int) → Int → (Int, Int)
moveCell _ c 0 = c
moveCell ((i0, j0), (im, jm)) (i, j) s
    | i ≡ i0 ∧ j < jm = moveCell ((i0, j0), (im, jm)) (i, j + 1) (s - 1)
    | i < im ∧ j ≡ jm = moveCell ((i0, j0), (im, jm)) (i + 1, j) (s - 1)
    | i ≡ im ∧ j > j0 = moveCell ((i0, j0), (im, jm)) (i, j - 1) (s - 1)
    | i > i0 ∧ j ≡ j0 = moveCell ((i0, j0), (im, jm)) (i - 1, j) (s - 1)

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '
    
    (m:n:r:_) ← getInts <$> BSC.getLine

    as ← join <$> (forM [1..m] $ \_ → getInts <$> BSC.getLine)

    let bnd = ((1,1),(m,n))
    let mx = listArray bnd as

    let mxr = rotate mx r

    forM_ [1..m] $ \i → do
        BSC.putStrLn $ BSC.intercalate " " $ map (\j → BSC.pack ∘ show $ mxr ! (i,j)) [1..n]

