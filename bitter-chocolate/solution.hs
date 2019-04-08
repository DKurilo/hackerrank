{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as DM
import Debug.Trace
import System.IO

data Result = WIN | LOSE
    deriving (Show, Eq, Ord)

data Player = A | B
    deriving (Show, Eq, Ord)

data State = GS [Int] Player
    deriving (Show, Eq, Ord)

type Cache = DM.Map State Result

nextPlayer ∷ Player → Player
nextPlayer A = B
nextPlayer B = A

canDo ∷ [Int] → [[Int]]
canDo [] = []
canDo rs = go 1 rs
    where go ∷ Int → [Int] → [[Int]]
          go _ [] = []
          go from (r':rs') = [x:lim x rs' | x ← [from..(r' - 1)]] ⧺
                             (map (r':) $ go 0 rs')
          lim ∷ Int → [Int] → [Int]
          lim m xs = map (\x → min m x) xs

winOrLose ∷ Cache → State → (Result, Cache)
winOrLose c st = case DM.lookup st c of
    Just r → (r, c)
    _ | head rs ≡ 1 ∧ filter (>0) rs ≡ [1] → (LOSE, DM.insert st LOSE c)
      | otherwise → (res, DM.insert st res c')
    where (GS rs p) = st
          (res, c') = foldl (\(r, c) rs' →
              let (r', c') = winOrLose c (GS rs' $ nextPlayer p) in
              if r' ≡ LOSE then (WIN,c') else (r,c')) (LOSE, c) $ canDo rs

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '
    n ← getInt <$> BSC.getLine

    forM_ [1..n] $ \_ → do
        rs ← getInts <$> BSC.getLine
        BSC.putStrLn ∘ BSC.pack ∘ show ∘ fst ∘ winOrLose DM.empty $ GS rs A

