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

-- Complete the minimumPasses function below.
minimumPasses :: Integer -> Integer -> Integer -> Integer -> Integer
minimumPasses m w p n = minimum $ minimumPasses' m w p n 0

minimumPasses' :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
minimumPasses' m w p n r
    | toprod == 1 || toprod <= tounit = [toprod]
    | otherwise = [toprod] ++ [tounit + (minimum $  minimumPasses' m' w' p n r')]
    where perday = m * w
          n' = n - r
          p' = p - r
          toprod | n' `mod` perday == 0 = n' `div` perday
                 | otherwise = 1 + n' `div` perday
          tounit | p' `mod` perday == 0 = p' `div` perday
                 | otherwise = 1 + p' `div` perday
          c = tounit * perday + r
          (canbuy, r') =  divMod c p
          (m',w') = divideEq canbuy m w

divideEq :: Integer -> Integer -> Integer -> (Integer,Integer)
divideEq c m w
    | m == w = (m + c2, w + c - c2)
    | m > w && c <= (m-w) = (m, w + c)
    | m > w = (m + c - w', w + w')
    | m < w && c <= (w-m) = (m + c, w)
    | m < w = (m + m', w + c - m')
    where c2 = c `div` 2
          w' = m - w + (c - m + w) `div` 2
          m' = w - m + (c - w + m) `div` 2

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    mwpnTemp <- getLine
    let mwpn = words mwpnTemp

    let m = read (mwpn !! 0) :: Integer

    let w = read (mwpn !! 1) :: Integer

    let p = read (mwpn !! 2) :: Integer

    let n = read (mwpn !! 3) :: Integer

    let result = minimumPasses m w p n

    -- hPutStrLn fptr $ show result
    putStrLn $ show result

    hFlush fptr
    hClose fptr

