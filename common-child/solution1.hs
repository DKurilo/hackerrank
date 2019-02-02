{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.List as DL
import Data.List.Split
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import qualified Data.Array.IArray as DA

-- Complete the commonChild function below.
type AString = DA.Array Int Char

commonChild :: String -> String -> Int
commonChild csn csm = commonChild' csn csm n m ((n+1)*(m+1)-1)
    where n = length csn
          m = length csm

commonChild' :: String -> String -> Int -> Int -> Int -> Int
commonChild' an am n m = (map field [0..] !!) 
    where field i = field' (i `div` (m+1)) (i `mod` (m+1))
          field' 0 _ = 0
          field' _ 0 = 0
          field' i j | an !! (i-1) == am !! (j-1) = trace ("e " ++ show i ++ " " ++ show j) $ 1 + commonChild' an am n m ((i-1)*(m+1)+(j-1))
                     | otherwise = trace ("n " ++ show i ++ " " ++ show j) $ max (commonChild' an am n m $ (i-1)*(m+1)+j) (commonChild' an am n m $ i*(m+1)+(j-1))

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


