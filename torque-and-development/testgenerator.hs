{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import qualified Data.List as DL
import Data.List.Split
import qualified  Data.Set as DS
import System.Environment
import System.IO
import qualified Data.Map.Strict as DM
import System.Random

import Debug.Trace

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode
    cn <- getStdRandom $ randomR (1,100000) :: IO Int
    rn <- getStdRandom $ randomR (1,min 100000 (cn * (cn -1) `div` 2)) :: IO Int
    l <- getStdRandom $ randomR (1,100000) :: IO Int
    r <- getStdRandom $ randomR (1,100000) :: IO Int
    
    hPutStrLn fptr $ "1"
    hPutStrLn fptr $ show cn ++ " " ++ show rn ++ " " ++ show l ++ " " ++ show r
    
    forM_ [1..rn] $ \_ -> do
        c1 <- getStdRandom $ randomR (1,cn) :: IO Int
        c2 <- getStdRandom $ randomR (1,cn) :: IO Int
        hPutStrLn fptr $ show c1 ++ " " ++ show c2

    hFlush fptr
    hClose fptr

