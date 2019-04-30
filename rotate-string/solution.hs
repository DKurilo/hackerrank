{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

rot ∷ String → [String]
rot cs = go cs $ length cs
    where go ∷ String → Int → [String]
          go _ 0 = [""] -- empty string
          go "" _ = [""] -- empty string
          go (c:cs') 1 = [cs' ⧺ [c]]
          go (c:cs') k = rcs:(go rcs $ k - 1)
              where rcs = cs' ⧺ [c]

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    t ← getInt <$> BSC.getLine
    forM_ [1..t] $ \_ → BSC.intercalate " " ∘ map (BSC.pack) ∘ rot ∘ BSC.unpack <$>
                        BSC.getLine ≫= BSC.putStrLn 

