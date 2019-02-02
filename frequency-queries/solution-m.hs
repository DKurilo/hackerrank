{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Array
import Data.Bits
import Data.ByteString.Char8 as BSC
import System.IO
import Debug.Trace
import System.Environment
import Data.Array.IO
import Data.Array.Base
import qualified Data.HashTable.IO as H

-- Complete the freqQuery function below.
type HashTable k v = H.CuckooHashTable k v

freqQuery :: Int -> IO ()
freqQuery n = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode
    hm <- H.new :: IO (HashTable Int Int)
    vs <- newArray (0,n) 0 :: IO (IOUArray Int Int)
    fs <- newArray (0,n) 0 :: IO (IOUArray Int Int)
    foldM (\(xm,i) _ -> do
            q <- BSC.getLine
            let (a:x:_) = Prelude.map (\w -> case readInt w of 
                    Just (j,_) -> j
                    _ -> 0) $ BSC.words q
            if a==3 then if x > (i - xm + 1) then do
                BSC.hPutStrLn fptr $ pack $ "0"
                return (xm,i) 
            else do
                f <- unsafeRead fs x
                BSC.hPutStrLn fptr $ (if f==0 then pack "0" else pack "1")
                return $ (xm,i)
            else do
                mx' <- H.lookup hm x
                let xm' = case mx' of
                        Just _ -> xm
                        _ -> xm + 1
                let x' = case mx' of
                        Just v -> v
                        _ -> xm
                case mx' of
                    Just _ -> return ()
                    _ -> H.insert hm x x'
                oldf <- unsafeRead vs x'
                let newf = if a==1 then oldf+1 else if oldf > 0 then oldf-1 else 0
                unsafeWrite vs x' newf
                tn <- unsafeRead fs newf
                unsafeWrite fs newf (tn+1)
                to <- unsafeRead fs oldf
                unsafeWrite fs oldf $ if to > 0 then to-1 else 0
                return $ (xm',i+1)
        ) (0,0) [1..n]
    hFlush fptr
    hClose fptr

main :: IO()
main = do

    qTemp <- BSC.getLine
    let q = case readInt qTemp of Just (v,_) -> v
                                  _ -> 0

    freqQuery q

