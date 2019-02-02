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
    stdout <- getEnv "STORE_PATH"
    fptr <- openFile stdout WriteMode
    let d = 2
    forM_ [1..d] $ \_ -> do
        n <- mapM (\_ -> getStdRandom $ randomR ('A','Z')) [1..20] :: IO String
        hPutStrLn fptr n
       
    hFlush fptr
    hClose fptr

