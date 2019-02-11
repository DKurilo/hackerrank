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

-- Complete the minTime function below.
data Answer = L | E | G deriving (Eq, Show)

search :: Int -> Int -> (Int -> Answer) -> Int
search from to p
    | to - from == 1 && (af == L) && (at == E || at == G) = to
    | to - from <= 1 && (af == E) && (at == E || at == G) = from
    | ac == L = search c to p
    | ac == G || ac == E = search from c p
    where c = (from + to) `div` 2
          ac = p c
          af = p from
          at = p to

production :: [(Int,Int)] -> Int -> Int
production ms d = foldl (\a (x,c) -> a + c * (d `div` x)) 0 $ ms

isEnough :: (Int -> Int) -> Int -> Int -> Answer
isEnough f g d
    | pr > g = G
    | pr == g = E
    | pr < g = L
    where pr = f d

moptimize :: [Int] -> [(Int, Int)]
moptimize ms = foldl (\((m', c):ms') m -> if m==m' then ((m,c+1):ms')
                                         else ((m,1):(m',c):ms')) [(head sms, 0)] sms
    where sms = DL.sort $ ms

minTime :: [Int] -> Int -> Int
minTime machines goal = search dmin dmax $ 
                        isEnough (production . moptimize $ machines) goal
    where ml = length machines
          slow = maximum machines
          fast = minimum machines
          cycles = (goal + goal `mod` ml) `div` ml
          dmin = cycles * fast
          dmax = cycles * slow + slow

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

    nGoalTemp <- getLine
    let nGoal = words nGoalTemp

    let n = read (nGoal !! 0) :: Int

    let goal = read (nGoal !! 1) :: Int

    machinesTemp <- getLine

    let machines = DL.map (read :: String -> Int) . words $ machinesTemp

    let ans = minTime machines goal

    -- hPutStrLn fptr $ show ans
    putStrLn $ show ans

    hFlush fptr
    hClose fptr

