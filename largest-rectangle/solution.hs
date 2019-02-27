{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the largestRectangle function below.
largestRectangle ∷ [Int] → Int
largestRectangle hs = largestRectangle' ∘ reverse $ hs

largestRectangle' ∷ [Int] → Int
largestRectangle' [] = 0
largestRectangle' (h:[]) = h
largestRectangle' (h:hs) = max (buildVertical (h:hs)) $ largestRectangle' hs

buildVertical ∷ [Int] → Int
buildVertical hs = buildVertical' Nothing 0 hs

buildVertical' ∷ Maybe Int → Int → [Int] → Int
buildVertical' _ _ [] = 0
buildVertical' m l (h:hs) =
    case m of
        Just x → let h' = min x h in max (h' * l') (buildVertical' (Just h') l' hs)
        Nothing → max (h * l') (buildVertical' (Just h) l' hs)
    where l' = l + 1

readMultipleLinesAsStringArray ∷ Int → IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line ← getLine
    rest ← readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main ∷ IO()
main = do
    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    n ← readLn ∷ IO Int

    hTemp ← getLine

    let h = map (read ∷ String → Int) ∘ words $ hTemp

    let result = largestRectangle h

    -- hPutStrLn fptr ∘ show $ result
    putStrLn ∘ show $ result

    hFlush fptr
    hClose fptr

