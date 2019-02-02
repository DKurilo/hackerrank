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

    n <- getStdRandom $ randomR (1,10000) :: IO Int
    k <- getStdRandom $ randomR (1,100) :: IO Int
    
    numbs <- sequence $ map (\_ -> getStdRandom $ randomR (1,1000000000)) [1..n] :: IO [Int] 
    
    hPutStrLn fptr $ (show n) ++ " " ++ (show k)
    hPutStrLn fptr $ (show . head $ numbs) ++ (concat $ Prelude.map (\n -> ' ':show n) $ tail numbs)
    hPutStrLn fptr $ ""
    
    -- putStrLn $ DL.intercalate "\n" $ DL.map (\x -> show x) $ result
    hFlush fptr
    hClose fptr


