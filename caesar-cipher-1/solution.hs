{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode -- so no we can use ←, ∷, etc
import Control.Monad.Unicode -- so now we can use ≫, ≫=, etc.
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.Environment
import System.IO
import Data.List (elemIndex)
import Data.Char (isUpper, toLower, toUpper)

abc = "abcdefghijklmnopqrstuvwxyz"
abcl = 26

caesarCipher ∷ String → Int → String
caesarCipher cs k = map (\c → let c' = toLower c
                                  mi = c' `elemIndex` abc
                                  u = isUpper c in
                              case mi of
                                  Nothing → c
                                  Just i → let c'' = abc !! ((i + k) `mod` abcl) in
                                           if u then toUpper c'' else c'') cs

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    _ ← BSC.getLine
    cs ← BSC.unpack <$> BSC.getLine
    k ← getInt <$> BSC.getLine

    BSC.putStrLn ∘ BSC.pack $ caesarCipher cs k
    -- BSC.hPutStrLn fptr ∘ BSC.pack $ caesarCipher cs k

    hFlush fptr
    hClose fptr
