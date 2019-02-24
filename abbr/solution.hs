{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import Data.Char
import qualified Data.Map as DM

-- Complete the abbreviation function below.
data Answer = YES|NO deriving (Eq, Show)
type AMap = DM.Map (Int,Int) Answer

abbreviation :: String -> String -> Answer
abbreviation a b = if a'' /= []  then NO
                   else fst $ abbreviation' a' b DM.empty
    where (a',a'') = foldr (\c (cs',cs'') ->
                if c == toUpper c && filter (==c) b == [] then (cs',(c:cs''))
                else if c == toUpper c then ((c:cs'),cs'')
                else if (or $ map (==(toUpper c)) b) then ((c:cs'),cs'')
                else (cs',cs'')) ([],[])  a

abbreviation' :: String -> String -> AMap -> (Answer, AMap)
abbreviation' as [] am = case DM.lookup (0,length as) am of
    Just ans -> (ans,am)
    _ -> 
        if filter (\c -> c == toUpper c) as == [] then (YES, DM.insert (0, length as) YES am)
        else (NO, DM.insert (0, length as) NO am)
abbreviation' [] bs am = (NO, am)
abbreviation' (a:as) (b:bs) am = case DM.lookupLE (1 + length bs,1+ length as) am of
    Just ((bsl,asl),ans) | bsl == (1 + length bs) -> (ans, am)
    _
        | try == YES -> (YES, DM.insert (1 + length bs,1 + length as) YES am')
        | toUpper a == a -> (NO, DM.insert (1 + length bs,1 + length as) NO am')
        | length as < 1 + length bs -> (NO, DM.insert (1 + length bs,1 + length as) NO am')
        | otherwise -> (nexttry, DM.insert (1 + length bs,1 + length as) nexttry am'')
    where (try, am')
              | toUpper a == b = abbreviation' as bs am
              | otherwise = (NO, am)
          (nexttry, am'') = abbreviation' as (b:bs) am'

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    q <- readLn :: IO Int

    forM_ [1..q] $ \q_itr -> do
        a <- getLine

        b <- getLine

        let result = abbreviation a b

        -- hPutStrLn fptr $ show result
        putStrLn $ show result

    hFlush fptr
    hClose fptr

