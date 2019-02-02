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
-- From bottom to top
-- In case stand-alone node, +1 in vertices count
-- In case explicit vertices count after adding increased, +1 to vertices count
-- In case it connected only  to nodes that can't be a vertices explicitly or not (preserving vertices count), +1 to vertices count
-- Otherwise, preserve vertices count 
--
data Node = N { nvi :: Int
              , nord :: Int
              , ncin :: Int
              , ncout :: Int
              , ncbv :: Bool
              } deriving (Eq,Show)

data State = ST { sns :: [Node]
                , svc :: Int
                , ssa :: Bool
                , svxc :: Bool
                } deriving (Show)

problemSolving :: Int -> [Int] -> Int
problemSolving k vis = svc $ foldr (addViToState k) (ST [] 0 True False) (zip (reverse vis) [0..]) -- trace (show q) $ q where q = 

--
-- New Vi is a vertex
-- If it connected to only one node that can be a vertex, this node is not a vertex anymore
-- If it connected to a node that is not vertex and amount of explicit vertex less then current pathes amount (svc) amount, do nothing
-- If it connected to multiple vertices, do nothing
-- If new amount of explicit vertices the same or more then current pathes amount, only explicit vertices can be vertices
--
-- TODO: add can be vertex (cbv) calculation!
--
addViToState :: Int -> (Int,Int) -> State -> State
addViToState k (vi,o) (ST ns vc sa vxc) = specialCaseVertices k $ updateVertices k $ ST (N vi o 0 (getcout k vi ns) True:sns st') vertices (ssa st') (svxc st') -- trace (show $ ST ns vc sa vxc) $ 
    where st' = foldr (\(N vi' o' cin' cout' cbv') (ST ns' vc' sa' vxc') -> 
                    if (abs $ vi - vi') >=k then ST (N vi' o' (cin'+1) cout' cbv':ns') vc' False (vxc' || cbv')
                    else ST (N vi' o' cin' cout' cbv':ns') vc' sa' vxc') (ST [] vc True False) ns
          vertices = if ssa st' then vc+1 else if newev > vc then newev else if (not . svxc $ st') then vc+1 else vc
          vcount = foldr (\(N _ _ cin _ _) a -> if cin == 0 then a+1 else a ) 1
          newev = vcount . sns $ st'

getcout :: Int -> Int -> [Node] -> Int
getcout k vi ns = foldr (\(N vi' _ _ _ _) a -> if (abs $ vi - vi') >= k then a+1 else a) 0 ns

updateVertices :: Int -> State -> State
updateVertices k (ST ns vc sa vxc)
    | sa = ST ns vc sa vxc
    | explicitVertices >= vc = ST (tn:(map (\(N vi ord cin cout cbv) -> N vi ord cin cout (if cin == 0 then True else False)) $ tail ns)) vc sa vxc
    | cvlength == 1 = ST (tn:(map (\(N vi ord cin cout cbv) -> N vi ord cin cout (if head cvertices == ord then False else cbv)) $ tail ns)) vc sa vxc
    | cvlength > 1 = ST ns vc sa vxc
    | cnvlength > 0 && explicitVertices < vc = ST ns vc sa vxc
    | otherwise = ST ns vc sa vxc
    where
        tn = head ns
        cvertices = foldr (\(N vi' ord' _ _ cbv') a -> if (abs $ vi' - nvi tn) >= k && cbv' == True then (ord':a) else a ) [] $ tail ns
        cvlength = length cvertices
        cnvlength = foldr (\(N vi' ord' _ _ cbv') a -> if vi' - nvi tn >= k && cbv' == False then a+1 else a ) 0 $ tail ns
        explicitVertices = foldr (\(N _ _ cin _ _) a -> if cin > 0 then a else a+1) 1 $ tail ns

specialCaseVertices :: Int -> State -> State
specialCaseVertices k (ST ns vc sa vxc) = ST (map (\n -> if ncin n > 1 && ncbv n == True then check n else n) ns) vc sa vxc
    where check (N vi ord cin cout cbv) = N vi ord cin cout (foldr (\(N vi' ord' cin' cout' cbv') a -> 
              if ord' < ord && (abs $ vi - vi') >= k && cin' == 0 then a+1 else a) 0 ns < 2)

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

        return ()

    hFlush fptr
    hClose fptr

