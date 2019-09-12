{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode -- so no we can use ←, ∷, etc
import Control.Monad.Unicode -- so now we can use ≫, ≫=, etc.
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.Set (fromList, toList, size)
import Debug.Trace
import System.Environment
import System.IO

-- function is here
alternate ∷ String → Int
alternate cs
    | size scs ≤ 1 = 0
    | size scs ≡ 2 ∧ isValid cs = length cs
    | size scs ≡ 2 = 0
    | otherwise = foldl max 0 ∘ map length ∘ filter isValid $
                  [filter (\c → c ≡ c1 ∨ c ≡ c2) cs | c1 ← ucs, c2 ← ucs, c1 ≠ c2]
    where scs = fromList cs
          ucs = toList scs

isValid ∷ String → Bool
isValid [] = True
isValid [_] = True
isValid (c1:c2:cs)
    | c1 ≡ c2 = False
    | otherwise = isValid (c2:cs)

main ∷ IO()
main = do
    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    _ ← BSC.getLine
    cs ← BSC.unpack <$> BSC.getLine

    BSC.putStrLn ∘ BSC.pack ∘ show ∘ alternate $ cs
    -- BSC.hPutStrLn fptr ∘ BSC.pack ∘ show ∘ alternate $ cs

    hFlush fptr
    hClose fptr
