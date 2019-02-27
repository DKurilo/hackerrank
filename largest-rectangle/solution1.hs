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
-- http://www.cse.unsw.edu.au/~carrollm/ProgrammingFromSpecifications.pdf

largestRectangle ∷ [Int] → Int
largestRectangle hs = fst ∘ largestRectangle' (-1) ∘ listArray (-1,length hs) $ 
                      (-1:hs) ⧺ [-1]

largestRectangle' ∷ Int → Array Int Int → (Int, Int)
largestRectangle' i hs = loop 0 (i + 1)
    where loop b j
              | hs ! j > hs ! i = 
                  let (c,k) = largestRectangle' j hs
                      (b',j') = (maximum [((k-i-1) * hs ! j), c, b], k)
                  in loop b' j'
              | otherwise = (b, j)

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


