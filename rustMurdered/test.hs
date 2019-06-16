{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.List
import Data.List.Split
import Data.Set
import System.Environment
import System.IO

--
-- Complete the rustMurdered function below.
--
data Step = S { sAll :: Set Int
              , sFront :: [Int]
              , sRes :: [Int]
              }

initStep :: Int -> Int -> Step
initStep r n = S (Data.Set.fromList [r]) 
                 [r]
                 (Prelude.take n $ repeat 0) 

justifyRoutes :: [Int] -> [Int]
justifyRoutes (x:y:_) | x < y = [x,y]
                      | otherwise = [y,x]

rustMurderer :: Int -> [[Int]] -> Int -> [Int]
rustMurderer n roads s = removeElement pathes s
    where rset = Data.Set.fromList . Prelude.take 300 . Prelude.map justifyRoutes $ roads
          pathes = sRes $ rustMurderer' rset n (initStep s n) 1

removeElement :: [a] -> Int -> [a]
removeElement [] _ = []
removeElement (_:xs) 1 = xs
removeElement (x:xs) i = x:removeElement xs (i-1)

rustMurderer' :: Set [Int] -> Int -> Step -> Int -> Step
rustMurderer' rset n step l
    | (size . sAll $ step) < n = rustMurderer' rset n (nextStep rset n step l) (l+1)
    | otherwise = step

nextStep :: Set [Int] -> Int -> Step -> Int -> Step
nextStep rset n step l = S (Data.Set.union (sAll step) asrs)
                           (Data.Set.toList asrs)
                           (replaceWithVal (sRes step) (Data.Set.toList asrs) l 1)
    where asrs = available rset n (sAll step) (sFront step)

replaceWithVal :: [Int] -> [Int] -> Int -> Int -> [Int]
replaceWithVal [] _ _ _ = []
replaceWithVal xs [] _ _ = xs
replaceWithVal (x:xs) (i:is) val ci | i == ci = (val:replaceWithVal xs is val (ci+1))
                                    | otherwise = (x:replaceWithVal xs (i:is) val (ci+1))

available :: Set [Int] -> Int -> Set Int -> [Int] -> Set Int
available rset n old front = Data.Set.map (\(x:xs) -> x) $ Data.Set.difference 
    (Data.Set.map (\(x:y:_) -> [y,x]) $ Data.Set.difference 
        (Data.Set.fromList [[x,y] | x <- front, y <- tlist, x /= y]) 
        rset
    ) rset
    where 
        tlist = Data.Set.toList $ Data.Set.difference (Data.Set.fromList [1..n]) old

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

    forM_ [1..t] $ \tItr -> do
        nmTemp <- getLine
        let nm = words nmTemp

        let n = read (nm !! 0) :: Int

        let m = read (nm !! 1) :: Int

        roadsTemp <- readMultipleLinesAsStringArray m
        let roads = Data.List.map (\x -> Data.List.map (read :: String -> Int) . words $ x) roadsTemp

        s <- readLn :: IO Int

        let result = rustMurderer n roads s

        hPutStrLn fptr $ intercalate " " $ Data.List.map (\x -> show x) $ result

    hFlush fptr
    hClose fptr

