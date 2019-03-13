{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the stepPerms function below.
stepPerms ∷ Int → Int
stepPerms n = steps n `mod` 10000000007

steps ∷ Int → Int
steps = (map sts [0..] !!)
    where sts 0 = 1
          sts 1 = 1
          sts 2 = 2
          sts k = steps (k-3) + steps (k-2) + steps (k-1)

main ∷ IO()
main = do
    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    s ← readLn ∷ IO Int

    forM_ [1..s] $ \s_itr → do
        n ← readLn ∷ IO Int

        let res = stepPerms n

        -- hPutStrLn fptr $ show res
        putStrLn $ show res

    hFlush fptr
    hClose fptr

