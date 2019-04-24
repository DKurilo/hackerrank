{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Sequence as S
import Debug.Trace
import System.IO

data WList = L (S.Seq Int) (S.Seq Int) -- left right
    deriving (Eq, Show)

data Work = W Int (S.Seq WList)
    deriving (Eq)

instance Show Work where
    show (W k wls) = show k --  ++ " - " ++ show wls

insert ∷ Int → Work → Work
insert a (W _ S.Empty) = W a (S.singleton $ L S.empty (S.singleton a))
insert a (W k (wl@(L ls rs) S.:<| owls)) = W (median wl') (wl' S.:<| wl S.:<| owls)
    where wl' = L ls' rs'
          r = S.length rs
          l = S.length ls
          plr = (S.length rs - S.length ls) < 2
          (ls'', rs'') = if a ≥ k then (ls, go rs) else (go ls,rs)
          (ls', rs') = if plr ∧ a ≥ k
                         then (ls'', rs'')
                         else if plr ∧ a < k
                           then (S.deleteAt l ls'', (S.singleton $ ls'' `S.index` l) S.>< rs'')
                           else if a ≥ k
                             then (ls'' S.>< (S.take 1 rs''), S.drop 1 rs'')
                             else (ls'', rs'')
          go ∷ S.Seq Int → S.Seq Int
          go as = a1 S.>< (S.singleton a) S.>< a2
              where (a1, a2) = S.splitAt (find 0 as) as
          find ∷ Int → S.Seq Int → Int
          find i as
              | S.length as ≡ 0 = i
              | S.length as ≡ 1 ∧ as `S.index` 0 ≤ a = i + 1
              | S.length as ≡ 1 = i
              | S.length as ≡ 2 ∧ as `S.index` 1 ≤ a = i + 2
              | S.length as ≡ 2 ∧ as `S.index` 0 ≤ a = i + 1
              | as `S.index` m ≡ a = m + i
              | as `S.index` m > a = find i $ S.take (m + 1) as
              | otherwise = find (m + i) $ S.drop m as
              where m = (S.length as - 1) `div` 2

median ∷ WList → Int
median (L _ S.Empty) = 0
median (L _ as) = as `S.index` 0

process ∷ Work → Int → Work
process (W k wls) a
    | a ≡ -1 = W k $ wl' S.:<| wls
    | a < 0 = W (median wl') $ wl' S.:<| wls
    | otherwise = insert a (W k wls)
    where wl' = wls `S.index` ((-a) - 1)

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    n ← getInt <$> BSC.getLine
    foldM_ (\w _ → do
            w' ← process w ∘ getInt <$> BSC.getLine
            BSC.putStrLn ∘ BSC.pack ∘ show $ w'
            return w'
        ) (W 0 S.empty) [1..n]

