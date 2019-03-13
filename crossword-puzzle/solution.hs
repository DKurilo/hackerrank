{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the crosswordPuzzle function below.
data Dir = V | H | Err
    deriving (Show, Eq)

crosswordPuzzle ∷ [String] → [String] → [String]
crosswordPuzzle cd ws = case solve H cd [] ws of
    (Err, _) → []
    (H, ls) → ls
    (V, ls) → transpose ls

solve ∷ Dir → [String] → [String] → [String] → (Dir, [String])
solve d ls rls [] = (d, reverse rls ⧺ ls)
solve H [] rls ws = solve V (transpose ∘ reverse $ rls) [] ws
solve V [] rls ws = (Err, [])
solve d (l:ls) rls (w:ws)
    | nls ≠ [] = case check nls of
                     (Err, _) → solve d ls (l:rls) (w:ws)
                     r → r
    | otherwise = solve d ls (l:rls) (w:ws)
    where nls = tryWord w l
          check ∷ [String] → (Dir, [String])
          check [] = (Err, [])
          check (nl:nls') = case solve H ls' [] ws of
                               (Err, _) → check nls'
                               r → r
              where dls = (reverse rls) ⧺ (nl:ls)
                    ls' = if d ≡ H
                           then dls
                           else transpose dls

tryWord ∷ String → String → [String]
tryWord w l = filter (≠"") $ getWords (split (condense $ onSublist "+") l) []
    where getWords ∷ [String] → [String] → [String]
          getWords [] _ = []
          getWords (w':ws) rws = if checkWord w w'
                                 then (join $ reverse rws ⧺ (w:ws)):getWords ws (w':rws)
                                 else getWords ws (w':rws)
          checkWord ∷ String → String → Bool
          checkWord [] [] = True
          checkWord _ [] = False
          checkWord [] _ = False
          checkWord _ ('+':_) = False
          checkWord (c:cs) (c':cs')
              | c' ≡ '-' ∨ c' ≡ c = checkWord cs cs'
              | otherwise = False

readMultipleLinesAsStringArray ∷ Int → IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line ← getLine
    rest ← readMultipleLinesAsStringArray (n - 1)
    return (line : rest)

main ∷ IO()
main = do
    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    crossword ← readMultipleLinesAsStringArray 10

    words ← endBy ";" <$> getLine

    let result = crosswordPuzzle crossword words

    hPutStrLn fptr $ intercalate "\n" result
    putStrLn $ intercalate "\n" result

    hFlush fptr
    hClose fptr

