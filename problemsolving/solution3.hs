{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.List
import Data.List.Split
import Data.Set
import System.Environment
import System.IO

--
-- Complete the problemSolving function below.
--
data Problem = P { pvi :: Int
                 , pord :: Int
                 } deriving (Show)

data Temp = T [Problem] | TEMPTY deriving (Show)

type Day = [Problem]

data State = ST { stdays :: [Day]
                , stps :: [Problem]
                , sttemp :: Temp
                } deriving (Show)

problemSolving :: Int -> [Int] -> Int
problemSolving k = length . stdays . fillDays k . initState

problemSolvingD :: Int -> [Int] -> State
problemSolvingD k = fillDays k . initState

initState :: [Int] -> State
initState vis = ST [[]] (Prelude.map (\(vi, o) -> P vi o) $ zip vis [1..]) TEMPTY

fillDays :: Int -> State -> State
fillDays _ (ST days [] TEMPTY) = ST days [] TEMPTY
fillDays k (ST days ps (T tps)) = fillDays k $ ST (placeReversedBlock k days tps) ps TEMPTY
fillDays k state = fillDays k $ placeProblem k state

placeReversedBlock :: Int -> [Day] -> [Problem] -> [Day]
placeReversedBlock _ days [] = days
placeReversedBlock _ [] tps = [reverse tps]
placeReversedBlock _ ([]:ds) tps = (reverse tps:ds)
placeReversedBlock k ((p:ps):ds) (tp:tps)
    | pord tp > pord tp && abs (pvi p - pvi tp) >= k = ([reverse (tp:tps)] ++ ((p:ps):ds))
    | otherwise = ((p:ps):placeReversedBlock k ds (tp:tps))

placeProblem :: Int -> State -> State
placeProblem k (ST (d:ds) (p:ps) TEMPTY) = ST (fst placed:ds) ps (snd placed)
    where placed = placeProblem' k d p TEMPTY
placeProblem _ state = state

placeProblem' :: Int -> Day -> Problem -> Temp -> (Day, Temp)
placeProblem' _ [] np t = ([np], t)
placeProblem' k (p:ps) np TEMPTY
    | abs (pvi p - pvi np) >= k = ((np:p:ps), TEMPTY)
    | otherwise = placeProblem' k ps np $ T [p]
placeProblem' k (p:ps) np (T tps)
    | abs (pvi p - pvi np) >= k = ((np:p:ps), T tps)
    | otherwise = placeProblem' k ps np $ T (p:tps)

--
-- Write your code here.
--

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

    t <- readLn :: IO Int

    replicateM_ t $ do
        nkTemp <- getLine
        let nk = words nkTemp

        let n = read (nk !! 0) :: Int

        let k = read (nk !! 1) :: Int

        vTemp <- getLine

        let v = Data.List.map (read :: String -> Int) . words $ vTemp

        let result = problemSolving k v

        -- hPutStrLn fptr $ show result
        putStrLn . show $ result

        return ()

    hFlush fptr
    hClose fptr

