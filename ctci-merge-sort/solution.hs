{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Set as DS
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the countInversions function below.
countInversions :: [Int] -> Int
countInversions arr = snd $ msort 1 $ map wrap $ arr

merge :: Ord a => Int -> [a] -> [a] -> ([a],Int)
merge _ [] [] = ([],0)
merge _ xs [] = (xs,0)
merge _ [] ys = (ys,0)
merge xl (x:xs) (y:ys) | x > y = (y:msl, nl+xl)
                       | otherwise = (x:msr, nr)
                           where (msl, nl) = merge xl (x:xs) ys
                                 (msr, nr) = merge (xl-1) xs (y:ys)

msort :: Ord a => Int -> [[a]] -> ([a],Int)
msort _ [] = ([],0)
msort _ [xs] = (xs,0)
msort n xss = (xss'', c' + c'')
    where (xss',c') = merge_pairs n xss
          (xss'',c'') = msort (n*2) xss'

merge_pairs :: Ord a => Int -> [[a]] -> ([[a]], Int)
merge_pairs _ [] = ([],0)
merge_pairs _ [xs] = ([xs],0)
merge_pairs n (xs:ys:xss) = (vs:vss,n'+n'')
    where (vs, n') = merge n xs ys 
          (vss, n'') = merge_pairs n xss

wrap :: a -> [a]
wrap a = [a]

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    t <- readLn :: IO Int

    forM_ [1..t] $ \t_itr -> do
        n <- readLn :: IO Int

        arrTemp <- BSC.getLine

        let arr = map (\w -> case BSC.readInt w of
                                         Just (j,_) -> j
                                         _ -> 0) $ BSC.words arrTemp

        let result = countInversions $! arr

        -- System.IO.hPutStrLn fptr $ show result
        System.IO.putStrLn $ show result

    hFlush fptr
    hClose fptr

