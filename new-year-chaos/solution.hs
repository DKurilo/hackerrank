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

import Debug.Trace

-- Complete the minimumBribes function below.
type Bribe = Maybe Int

showb :: Bribe -> String
showb Nothing = "Too chaotic"
showb (Just n) = show n

minimumBribes :: [Int] -> Bribe
minimumBribes q = minimumBribes' (zip q [1..]) (length q) [] 0

minimumBribes' :: [(Int,Int)] -> Int -> [Int] -> Int -> Bribe
minimumBribes' [] _ _ _ = Just 0
minimumBribes' ((n,i):rq) lq pq lpq
    | n-i>2 = Nothing
    | otherwise = fmap (+(foldr (\k a-> if k > n then a+1 else a) 0 $ take (lpq - n + 3) $ pq)) $ minimumBribes' rq (lq-1) (n:pq) (lpq+1)

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    t <- readLn :: IO Int

    forM_ [1..t] $ \t_itr -> do
        n <- readLn :: IO Int

        qTemp <- getLine

        let q = DL.map (read :: String -> Int) . words $ qTemp

        putStrLn.showb $ minimumBribes q

