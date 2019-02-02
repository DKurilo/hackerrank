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

    n <- getStdRandom $ randomR (99999,100000) :: IO Int
    m <- getStdRandom $ randomR (99999,100000) :: IO Int
    
    routes <- sequence $ map (\i -> 
        fmap (\rnd -> (i,rnd)) $ 
            getStdRandom $ randomR (1,(i-1))) [2..n]
   
    trips <- sequence $ map (\_ -> pair (getStdRandom $ randomR (1,n))
                                        (getStdRandom $ randomR (999999999,1000000000))) [1..m] 
    
    hPutStrLn fptr $ DL.intercalate "\n" $ DL.map (\(x,y) -> show x ++ " " ++ show y) $
        ((n,m):routes++trips)
    -- putStrLn $ DL.intercalate "\n" $ DL.map (\x -> show x) $ result
    hFlush fptr
    hClose fptr


