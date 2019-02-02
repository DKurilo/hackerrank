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
                 , pc :: Int
                 , por :: Int
                 } deriving (Show, Eq)

problemSolving :: Int -> [Int] -> Int
problemSolving k ps = countVertices $ normalize k $ processConnections k $
        Prelude.map (\(v,o) -> P v 0 o) $ zip ps [1..]

problemSolvingD :: Int -> [Int] -> [Problem]
problemSolvingD k ps = normalize k $ processConnections k $
        Prelude.map (\(v,o) -> P v 0 o) $ zip ps [1..]

countVertices :: [Problem] -> Int
countVertices = Prelude.foldr (\p a -> if pc p == 0 then a+1 else a) 0

processConnections :: Int -> [Problem] -> [Problem]
processConnections k ps = reverse $ processConnections' k $ reverse ps

processConnections' :: Int -> [Problem] -> [Problem]
processConnections' _ [] = []
processConnections' k (v:vs) = (
    Prelude.foldr 
        (\(P el' _ _) (P el c o) -> 
            if (abs $ el' - el) >= k then (P el (c+1) o) else (P el c o)) (P (pvi v) 0 (por v)) vs:
    processConnections' k vs)

simplify :: Int -> [Problem] -> [Problem]
simplify k ps = simplify' k ps [] (countVertices $ processConnections' k $ reverse ps)

simplify' :: Int -> [Problem] -> [Problem] -> Int -> [Problem]
simplify' k [] sps _ = reverse $ processConnections' k sps
simplify' k (p:ps) sps v
    | pc p == 0 && vWithout >= v = simplify' k ps sps vWithout
    | otherwise = simplify' k ps (p:sps) v
    where vWithout = countVertices $ processConnections' k $ (reverse ps) ++ sps

normalize :: Int -> [Problem] -> [Problem]
normalize _ [] = []
normalize k (p:ps) = (p:normalize k (Prelude.map (\x -> if elem x elist then P (pvi x) (pc x-1) (por x) else x) ps))
    where elist = excludeMin $ getIn k p ps

getIn :: Int -> Problem -> [Problem] -> [Problem]
getIn _ _ [] = []
getIn k cp (p:ps) | (abs $ pvi cp - pvi p) >= k = (p:getIn k cp ps)
                  | otherwise = getIn k cp ps

excludeMin :: [Problem] -> [Problem]
excludeMin ps = excludeMin' (getMin ps) ps

excludeMin' :: Int -> [Problem] -> [Problem]
excludeMin' _ [] = []
excludeMin' m (p:ps) | m == pc p = ps
                     | otherwise = (p:excludeMin' m ps)

getMin :: [Problem] -> Int
getMin ps = minimum $ Prelude.map pc ps


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

