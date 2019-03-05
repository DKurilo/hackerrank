{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
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

-- Complete the poisonousPlants function below.
type Day = Int
type Pesticide = Int
data Plant = P Pesticide Day
    deriving (Eq, Show)
day ∷ Plant → Day
day (P p d) = d
pest ∷ Plant → Int
pest (P p d) = p

poisonousPlants ∷ [Int] → Int
poisonousPlants [] = 0
poisonousPlants ps = maximum ∘ map day ∘ poisonousPlants' p1 [P p1 0] [P p1 0] ∘ tail $ ps
    where p1 = head ps

poisonousPlants' ∷ Pesticide → [Plant] → [Plant] → [Pesticide] → [Plant]
poisonousPlants' _ _ pps [] = pps
poisonousPlants' ll (l:ls) (p:pps) (n:nps)
    | n > pest p = poisonousPlants' ll (l:ls) (P n 1:p:pps) nps
    | n ≤ ll = poisonousPlants' n [P n 0] (P n 0:p:pps) nps
    | n ≤ pest l = poisonousPlants' ll (P n d:l':ls') (P n d:p:pps) nps
    | n ≤ pest p = poisonousPlants' ll (P n (day p + 1):l:ls) (P n (day p + 1):p:pps) nps
    where ((l':ls'), m') = processLimits n (l:ls) 0
          d = 1 + maximum [day p, m']
          processLimits ∷ Pesticide → [Plant] → Day  → ([Plant],Day)
          processLimits p'' (l'':[]) md = ([l''], max md $ day l'')
          processLimits p'' (l'':ls'') md
              | pest l'' ≥ p'' = processLimits p'' ls'' $ max md $ day l''
              | otherwise = ((l'':ls''), md)

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

    n <- readLn :: IO Int

    pTemp <- getLine

    let p = DL.map (read :: String -> Int) . words $ pTemp

    let result = poisonousPlants p

    putStrLn $ show result
    -- hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr

