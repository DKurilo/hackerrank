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
    -- d <- getStdRandom $ randomR (1,100000) :: IO Int
    let d = 10
    hPutStrLn fptr $ show d
    arr <- mapM (\_ -> getStdRandom $ randomR (1,10) :: IO Int) [1..d]
    hPutStrLn fptr $ DL.intercalate " " $ map show $ arr

    hFlush fptr
    hClose fptr

