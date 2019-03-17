{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Data.List.Split
import Data.List
import qualified Data.ByteString.Char8 as BSC
import qualified Data.IntSet as DS
import Debug.Trace
import System.Environment
import System.IO

-- Complete the maxCircle function below.
maxCircle ∷ [[Int]] → [Int]
maxCircle queries = reverse ∘ (\(_,_,rs) → rs) ∘ foldl' process ([], DS.empty, []) $ queries

process ∷ ([DS.IntSet], DS.IntSet, [Int]) → [Int] → ([DS.IntSet], DS.IntSet, [Int])
process (ss,cs,rs) (x1:x2:_)
    | is1 ∧ is2 = (u1s, cs, (max u1c m:rs))
    | is1 = (u2s, DS.insert x2 cs, (max u2c m:rs))
    | is2 = (u3s, DS.insert x1 cs, (max u3c m:rs))
    | otherwise = ((DS.fromList [x1,x2] : ss), DS.insert x1 $ DS.insert x2 cs, (max 2 m:rs))
    where is1 = DS.member x1 cs
          is2 = DS.member x2 cs
          m = if rs ≠ [] then head rs else 0
          (u1s,u1c) = step1 ss DS.empty [x1,x2]
          (u2s,u2c) = step2 x1 x2 ss
          (u3s,u3c) = step2 x2 x1 ss
          step1 ∷ [DS.IntSet] → DS.IntSet → [Int] → ([DS.IntSet], Int)
          step1 ss' u [] = (u:ss', DS.size u)
          step1 [] u xs = ([u], DS.size u)
          step1 (s':ss') u xs
              | u' ≡ u = (s':ss'', c)
              | otherwise = (ss'', c)
              where (ss'',c) = step1 ss' u' xs'
                    (u', xs') = foldl' (\(sc, xa) x → if DS.member x s'
                                                      then (DS.union sc s',xa)
                                                      else (sc,x:xa)) (u, []) xs
          step2 ∷ Int → Int → [DS.IntSet] → ([DS.IntSet], Int)
          step2 _  _ [] = ([], 0)
          step2 x1' x2' (s':ss')
              | DS.member x1 s' = (DS.insert x2' s': ss',1 + DS.size s')
              | otherwise = (s':ss'', c)
              where (ss'',c) = step2 x1' x2' ss'

readMultipleLinesAsStringArray ∷ Int → IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line ← getLine
    rest ← readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    q ← getInt <$> BSC.getLine

    queries ← forM [1..q] $ \_ → getInts <$> BSC.getLine

    let ans = maxCircle queries

    BSC.putStrLn $ BSC.intercalate "\n" $ map (\x → BSC.pack ∘ show $ x) $ ans
    -- BSC.hPutStrLn fptr $ BSC.intercalate "\n" $ map (\x → BSC.pack ∘ show $ x) $ ans

    hFlush fptr
    hClose fptr

