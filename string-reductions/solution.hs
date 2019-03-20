{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.Environment
import System.IO

deDupe ∷ String → String
deDupe = reverse ∘ fst ∘ foldl (\(cs,ds) c → if c `elem` ds
                                             then (cs,ds)
                                             else (c:cs,c:ds)) ("","")

main ∷ IO()
main = do
    cs ← BSC.unpack <$> BSC.getLine
    let ans = deDupe cs
    BSC.putStrLn ∘ BSC.pack $ ans

