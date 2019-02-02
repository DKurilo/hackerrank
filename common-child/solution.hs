{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

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
import Data.Map.Strict as DM

-- Complete the commonChild function below.
type CCMap = DM.Map (String,String) Int

commonChild :: String -> String -> Int
commonChild cs1 cs2 = fst $ commonChild' cs1 cs2 DM.empty

commonChild' :: String -> String -> CCMap -> (Int, CCMap)
commonChild' [] _ m = (0, m)
commonChild' _ [] m = (0, m)
commonChild' (c1:cs1) (c2:cs2) m = 
    case DM.lookup ((c1:cs1),(c2:cs2)) m of
        Just n -> (n,m)
        _ | c1 == c2 -> (1 + n', DM.insert ((c1:cs1),(c2:cs2)) (1+n') m')
          | otherwise -> (n'', DM.insert ((c1:cs1),(c2:cs2)) n'' m2)
    where (n', m') = commonChild' cs1 cs2 m
          (n1,m1) = commonChild' (c1:cs1) cs2 m
          (n2,m2) = commonChild' cs1 (c2:cs2) m1
          n'' = max n1 n2

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s1 <- getLine

    s2 <- getLine

    let result = commonChild s1 s2

    -- hPutStrLn fptr $ show result
    putStrLn $ show result

    hFlush fptr
    hClose fptr

