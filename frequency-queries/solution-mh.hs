{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import qualified Data.HashTable.IO as H

-- Complete the freqQuery function below.
type HashTable k v = H.CuckooHashTable k v

freqQuery :: [[Int]] -> IO [Int]
freqQuery queries = freqQuery' queries H.new H.new

freqQuery' :: [[Int]] -> IO (HashTable Int Int) -> IO (HashTable Int Int) -> IO ([Int])
freqQuery' [] _ _ = return []
freqQuery' ((3:x:_):qs) mvs mfs = do
    fs <- mfs
    mf <- H.lookup fs x
    let f = case mf of Just _ -> 1
                       _ -> 0
    fmap (\v -> (f:v)) $ freqQuery' qs mvs $ return fs
freqQuery' ((a:x:_):qs) mvs mfs = do
    vs <- mvs
    fs <- mfs
    moldf <- H.lookup vs x
    let mnewf = if a==1 then 
             case moldf of Just oldf -> Just (oldf+1)
                           _ -> Just 1
        else case moldf of Just oldf -> if oldf>1 then Just (oldf-1) else Nothing
                           _ -> Nothing
    case mnewf of 
        Just newf -> do
            H.insert vs x newf
            mtn <- H.lookup fs newf
            let ntn = case mtn of Just tn -> (tn+1)
                                  _ -> 1
            H.insert fs newf ntn
        _ -> do
            H.delete vs x
    case moldf of
        Just oldf -> do
            mto <- H.lookup fs oldf
            case mto of 
                Just to -> 
                    if to>1 then H.insert fs oldf (to-1) else H.delete fs oldf
                _ -> return ()
        _ -> return ()
    freqQuery' qs (return vs) (return fs)
freqQuery' _ _ _ = return []

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    qTemp <- getLine
    let q = read $ lstrip $ rstrip qTemp :: Int

    queriesTemp <- readMultipleLinesAsStringArray q
    let queries = DL.map (\x -> DL.map (read :: String -> Int) . DL.words $ rstrip x) queriesTemp

    ans <- freqQuery queries

    hPutStrLn fptr $ DL.intercalate "\n" $ DL.map (\x -> show x) $ ans
    -- putStrLn $ DL.intercalate "\n" $ DL.map (\x -> show x) $ ans

    hFlush fptr
    hClose fptr

