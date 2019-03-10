{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Set as DS
import qualified Data.Map.Strict as DM
import qualified Data.List as DL
import System.Environment
import System.IO
import Debug.Trace

-- Complete the minTime function below.

data City = C Bool [(Int,Int)]
    deriving (Show, Eq)
type Roads = DM.Map Int City

addCity ∷ City → City → City
addCity (C m1 cs1) (C m2 cs2) = C (m1 ∨ m2) (cs1⧺cs2)

minTime ∷ [[Int]] → [Int] → Int
minTime roads machines = minTime' [buildG roads $ DS.fromList machines] machines

minTime' ∷ [Roads] → [Int] → Int
minTime' _ [] = 0
minTime' rss (m:ms)
    | found = cost + minTime' rss' (m:ms)
    | otherwise = minTime' rss ms
    where (found, (cost, rss')) = 
              DL.foldl' (\(fd, (c, rss'')) rs →
                            if m `DM.member` rs
                            then (True, ((\(c',rss2) → (c', rss2⧺rss'')) $ split m rs))
                            else (fd, (c, rs:rss''))
                        ) (False, (0, [])) rss

buildG ∷ [[Int]] → DS.Set Int → Roads
buildG rs ms =
    DL.foldl' (\rss (c1:c2:cost:_) → 
        DM.insertWith addCity c2 (C (c2 `DS.member` ms) [(c1,cost)]) $ 
        DM.insertWith addCity c1 (C (c1 `DS.member` ms) [(c2,cost)]) rss)
        DM.empty rs

split ∷ Int → Roads → (Int, [Roads])
split m rs = case cut of
    Just (cost, c1, c2) → (cost, [part rs c1 c2, part rs c2 c1])
    _ → (0, [])
    where cut = cutOut m rs

cutOut ∷ Int → Roads → Maybe (Int, Int, Int)
cutOut m rs = cutOut' m rs cs
    where (C _ cs) = rs DM.! m

cutOut' ∷ Int → Roads → [(Int,Int)] → Maybe (Int, Int, Int)
cutOut' _ _ [] = Nothing
cutOut' m rs ((c,cost):cs)
    | ism = Just (cost, m, c)
    | otherwise = case step of
        Nothing → cutOut' m rs cs
        Just (cost', _, _) → if cost < cost'
                                 then Just (cost, m, c)
                                 else step
    where (C ism cs') = rs DM.! c
          step = cutOut' c rs $ foldr (\(c',cost') acs → if c' ≡ m
                                                        then acs
                                                        else (c',cost'):acs) [] cs'

part ∷ Roads → Int → Int → Roads
part rs s r = part' rs (DM.singleton s (C ism $ filter (\(c,_) → c ≠ r) cs)) cs'
    where (C ism cs) = rs DM.! s
          cs' = foldr (\(c,_) acs → if c ≡ r
                                    then acs
                                    else c:acs) [] cs

part' ∷ Roads → Roads → [Int] → Roads
part' _ prs [] = prs
part' rs prs cs = part' rs prs' cs'
    where (prs', cs') = DL.foldl' (\(prs'', cs'') c → 
              let (C ism ccs) = rs DM.! c in 
                  (DM.insert c (C ism ccs) prs'', 
                  foldr (\(cc,_) acs → if cc ≡ c ∨ cc `DM.member` prs''
                                       then acs
                                       else cc:acs) cs'' ccs))
              (prs,[]) cs

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    stdout ← getEnv "OUTPUT_PATH"

    fptr ← openFile stdout WriteMode

    (n:k:_) ← getInts <$> BSC.getLine

    roads ← forM [1..n - 1] $ \_ → getInts <$> BSC.getLine
    machines ← mapM (\_ → getInt <$> BSC.getLine) [1..k]

    let result = minTime roads machines

    BSC.putStrLn ∘ BSC.pack ∘ show $ result
    -- BSC.hPutStrLn fptr ∘ BSC.pack ∘ show $ result

    hFlush fptr
    hClose fptr

