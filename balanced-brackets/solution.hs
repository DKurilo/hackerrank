{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the isBalanced function below.
data Answer = YES|NO deriving (Show)

isBalanced ∷ String → Answer
isBalanced = isBalanced' ""

isBalanced' ∷ String → String → Answer
isBalanced' "" "" = YES
isBalanced' _ "" = NO
isBalanced' "" (c:cs)
    | isOpenBracket c = isBalanced' [c] cs
    | isCloseBracket c = NO
    | otherwise = isBalanced' "" cs
isBalanced' (b:bs) (c:cs)
    | isOpenBracket c = isBalanced' (c:b:bs) cs
    | isCloseBracket c ∧ isPairBracket b c = isBalanced' bs cs
    | isCloseBracket c = NO
    | otherwise = isBalanced' (b:bs) cs

isOpenBracket ∷ Char → Bool
isOpenBracket '(' = True
isOpenBracket '[' = True
isOpenBracket '{' = True
isOpenBracket _ = False

isCloseBracket ∷ Char → Bool
isCloseBracket ')' = True
isCloseBracket ']' = True
isCloseBracket '}' = True
isCloseBracket _ = False

isPairBracket ∷ Char → Char → Bool
isPairBracket '(' ')' = True
isPairBracket '{' '}' = True
isPairBracket '[' ']' = True
isPairBracket _ _ = False

main ∷ IO()
main = do
    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    t <- readLn ∷ IO Int

    forM_ [1..t] $ \t_itr → do
        s ← getLine

        let result = isBalanced s

        hPutStrLn fptr ∘ show $ result

    hFlush fptr
    hClose fptr

