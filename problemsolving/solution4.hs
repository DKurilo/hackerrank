{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import System.Environment
import System.IO
import qualified Data.Map as DM

import Debug.Trace

--
-- Complete the problemSolving function below.
--

data Problem = P { viord :: Int
                 , vival :: Int
                 } deriving (Show, Eq, Ord)

type PList = [Problem]

type Path = [Problem]

data Node = N { np :: Problem
              , nin :: PList
              , nout :: PList
              } deriving (Eq, Show)
type Graph = [Node]

buildPList :: [Int] -> PList
buildPList vis = map (\(vi, o) -> P o vi) $ zip vis [1..]

buildGraph :: Int -> PList -> (Graph, Int)
buildGraph k ps = buildGraph' k ps [] 0

buildGraph' :: Int -> PList -> PList -> Int -> (Graph, Int)
buildGraph' _ [] _ rem = ([], rem)
buildGraph' k (p:ps) tps rem
    | cin == [] && cout == [] = buildGraph' k ps tps (rem+1)
    | otherwise = (\(g, r) -> ((N p cin cout:g), r)) $ buildGraph' k ps (p:tps) rem 
    where checkAdd = (\x a -> if abs (vival x - vival p) >= k then (x:a) else a)
          cin = foldr checkAdd [] tps
          cout = foldr checkAdd [] ps

getTopsBottoms :: Graph -> (PList, PList)
getTopsBottoms = (\(t,b) -> (map np $ DL.sortBy (\a b -> compare (nout a) (nout b)) t, map np b)) . (getTopsBottoms' [] [])

getTopsBottoms' :: Graph -> Graph -> Graph -> (Graph,Graph)
getTopsBottoms' tops bottoms [] = (tops, bottoms)
getTopsBottoms' tops bottoms (n:ns) = getTopsBottoms' 
        (if (length . nin $ n) == 0 then (n:tops) else tops)
        (if (length . nout $ n) == 0 then (n:bottoms) else bottoms)
        ns

processPList :: Int -> PList -> Int
processPList _ [] = 0
processPList k ps 
    | (length . fst $ tsbs) == length ps = snd gr + length ps
    | fst gr == [] = snd gr
    | otherwise =  snd gr + 1 + (processPathes k pathes $ map np . fst $ gr)
    where gr = buildGraph k ps
          tsbs = getTopsBottoms . fst $ gr
          p = head . fst $ tsbs
          pathes = findPathes (fst gr) p

processPathes :: Int -> [Path] -> PList -> Int
processPathes k [] _ = 0
processPathes k (path:pathes) ps = processPathes' k pathes ps $ processPList k $ removePath path ps

processPathes' :: Int -> [Path] -> PList -> Int -> Int
processPathes' _ [] _ td = td
processPathes' _ _ _ 1 = 1
processPathes' k (path:pathes) ps td = processPathes' k pathes ps $ min td $ processPList k $ removePath path ps

removePath :: Path -> PList -> PList
removePath _ [] = []
removePath path (p:ps) | elem p path = removePath path ps
                       | otherwise = (p:removePath path ps)

findPathes :: Graph -> Problem -> [Path]
findPathes graph p = fst $ findPathes' graph p DM.empty

findPathes' :: Graph -> Problem -> DM.Map Problem [Path] -> ([Path], DM.Map Problem [Path])
findPathes' graph p hm = case foundInHashMap of
    Just ps -> (ps, hm)
    Nothing -> if nodes == [] then ([[p]], DM.insert p [[p]] hm) 
               else (npathes, DM.insert p npathes $ snd found)
    where nodes = nout $ findNode graph p
          found = foldr (\x (a,hm') -> 
                  (\(ps, hm'') -> (ps ++ a, hm'')) $ findPathes' graph x hm') ([], hm) $ nodes
          foundInHashMap = DM.lookup p hm
          npathes = map (\x -> p:x) $ fst found

findNode :: Graph -> Problem -> Node
findNode graph p = head $ foldr (\n a -> if p == np n then (n:a) else a) [] graph

problemSolving :: Int -> [Int] -> Int
problemSolving k vis = processPList k $ buildPList vis

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

        let v = DL.map (read :: String -> Int) . words $ vTemp

        let result = problemSolving k v

        -- hPutStrLn fptr $ show result
        putStrLn . show $ result
        -- putStrLn . show $ snd $ (\(g,r) -> (optimizeGraph k g,r)) $ buildGraph k $ buildPList v
        -- putStrLn . show $ map (vival.np) . fst $ (\(g,r) -> (optimizeGraph k g,r)) $ buildGraph k $ buildPList v

        return ()

    hFlush fptr
    hClose fptr

