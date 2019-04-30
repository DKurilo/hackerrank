{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.Set (Set, empty, insert, notMember)
import Debug.Trace
import System.IO

unique ∷ String → String
unique = go empty
    where go ∷ Set Char → String → String
          go _ [] = []
          go s (c:cs)
              | c `notMember` s = c:go (insert c s) cs
              | otherwise = go s cs

main ∷ IO()
main = do
    BSC.pack ∘ unique ∘ BSC.unpack <$> BSC.getLine ≫= BSC.putStrLn

