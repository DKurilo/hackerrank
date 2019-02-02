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
    let d = 15
    hPutStrLn fptr $ show d
    forM_ [1..d] $ \_ -> do
        n <- getStdRandom $ randomR (1,100000) :: IO Int
        hPutStrLn fptr $ show n
       
        arr <- mapM (\_ -> getStdRandom $ randomR (1,10000000) :: IO Int) [1..n]
        hPutStrLn fptr $ DL.intercalate " " $ map show $ arr

    hFlush fptr
    hClose fptr



