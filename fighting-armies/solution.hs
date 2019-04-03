{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.IntMap.Strict as DM
import Debug.Trace
import System.IO

action ∷ DM.IntMap (DM.IntMap Int) → [Int] → (Maybe Int, DM.IntMap (DM.IntMap Int))
action as (1:i:_) = if as ≡ DM.empty
                      then (Just 0, as)
                      else (Just (fst ∘ DM.findMax $ as DM.! i), as)
action as (2:i:_) = (Nothing, DM.update doit i as)
    where doit ∷ DM.IntMap Int → Maybe (DM.IntMap Int)
          doit m = Just $ DM.updateMax (\n → if n > 1 then Just (n - 1) else Nothing) m
action as (3:i:c:_) = (Nothing, DM.insertWith doit i (DM.singleton c 1) as)
    where doit ∷ DM.IntMap Int → DM.IntMap Int → DM.IntMap Int
          doit m1 m2 = DM.unionWith (+) m1 m2
action as (4:i:j:_) =
    (Nothing, (DM.delete j $ DM.insert i (DM.unionWith (+) (as DM.! i) (as DM.! j)) as))
action as _ = (Nothing, as)

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    (n:q:_) ← getInts <$> BSC.getLine

    foldM_ (\as _ → do
        (mo, as') ← action as ∘ getInts <$> BSC.getLine
        case mo of
            Just o → BSC.putStrLn ∘ BSC.pack ∘ show $ o
            _ → return ()
        return $! as') DM.empty [1..q]

