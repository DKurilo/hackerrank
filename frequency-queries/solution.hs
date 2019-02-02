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
import qualified Data.IntMap.Strict as DM

-- Complete the freqQuery function below.
freqQuery :: [[Int]] -> [Int]
freqQuery queries = freqQuery' queries DM.empty DM.empty

freqQuery' :: [[Int]] -> DM.IntMap Int -> DM.IntMap Int -> [Int]
freqQuery' [] _ _ = []
freqQuery' ((1:x:_):qs) mv mf = freqQuery' qs mv' $! mf'
    where oldf = case DM.lookup x mv of
                     Just f' -> f'
                     _ -> 0
          newf = oldf + 1
          mv' = DM.insertWith (+) x 1 mv
          mf' = DM.insertWith (+) newf 1 $ minus1 oldf mf
freqQuery' ((2:x:_):qs) mv mf = freqQuery' qs mv' $! mf'
    where oldf = case DM.lookup x mv of
                     Just f' -> f'
                     _ -> 0
          newf = if oldf > 1 then oldf - 1 else 0
          mv' = if oldf == 0 then mv else minus1 x mv
          mf' = if oldf == 0 then mf else minus1 oldf $ 
                    if newf == 0 then mf else DM.insertWith (+) newf 1 mf
freqQuery' ((3:x:_):qs) mv mf = ((case DM.lookup x mf of
      Just f -> 1
      _ -> 0):freqQuery' qs mv mf)
freqQuery' _ _ _ = []

minus1 :: Int -> DM.IntMap Int -> DM.IntMap Int
minus1 i m = DM.update (\a -> if a-1 <= 0 then Nothing else Just (a-1)) i m

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

    let ans = freqQuery queries

    -- hPutStrLn fptr $ DL.intercalate "\n" $ DL.map (\x -> show x) $ ans
    putStrLn $ DL.intercalate "\n" $ DL.map (\x -> show x) $ ans

    hFlush fptr
    hClose fptr

