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

pair :: IO a -> IO b -> IO (a,b)
pair ma mb = do
    a <- ma
    b <- mb
    return (a,b)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    q <- getStdRandom $ randomR (1,100000) :: IO Int
    hPutStrLn fptr $ (show q)
    forM_ [1..q] $ \_ -> do
        p1 <- getStdRandom $ randomR (1,1000000000) :: IO Int
        p2 <- getStdRandom $ randomR (1,1000000000) :: IO Int
        hPutStrLn fptr $ (show p1) ++ " " ++ (show p2)
    
    hFlush fptr
    hClose fptr


