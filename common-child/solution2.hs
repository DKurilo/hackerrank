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
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Base

-- Complete the commonChild function below.
commonChild :: String -> String -> Int
commonChild csn csm = runST $ do
    let n = length csn
    let m = length csm
    an <- newListArray (0,n) (' ':csn) :: ST s (STUArray s Int Char)
    am <- newListArray (0,m) (' ':csm) :: ST s (STUArray s Int Char)
    sm <- newArray (0,(n+1)*(m+1)-1) (-1) :: ST s (STUArray s Int Int)
    let field = \i j -> if i == 0 || j == 0 then return 0 else do
                    cached <- unsafeRead sm (i*(m+1)+j)
                    if cached >= 0 then return cached else do
                        cn <- unsafeRead an i
                        cm <- unsafeRead am j
                        res <- if cn == cm then do 
                                   r <- field (i-1) (j-1)
                                   return $ 1 + r
                               else do
                                   r1 <- field (i-1) j
                                   r2 <- field i (j-1)
                                   return $ max r1 r2 
                        unsafeWrite sm (i*(m+1)+j) res
                        return res
    res <- field n m
    tan <- unsafeRead an n
    trace (show tan) $ return ()
    forM_ [0..n] $ \i -> do
        line <- foldr (\j ma -> do
            v <- unsafeRead sm (i*(m+1)+j)
            a <- ma
            return $ show (if v==(-1) then 0 else v) ++ " " ++ a) (return "") [0..m]
        trace line $ return ()
    return $ res 

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



