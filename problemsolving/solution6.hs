{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import System.Environment
import System.IO
import qualified Data.Map.Strict as DM

import Debug.Trace

--
-- Complete the problemSolving function below.
--
data Visited = From Int | Not deriving (Show, Eq, Ord)
data Node = N { nord :: Int
              , ns :: [Int]
              , nc :: Visited -- current cycle
              , nv :: Visited -- finally
              } deriving (Show, Eq, Ord)

data State = ST { sns :: DM.Map Int Node
                , sstop :: Bool
                , send :: Bool
                } deriving (Show)

problemSolving :: Int -> [Int] -> Int
problemSolving k vis = fst $ 
            foldl (\(a, st) i -> let found = findPath st i i in
                    (a+(if (send $ found) == True then 1 else 0), found))
                (0, generateState k $ zip vis [0..]) [0..(length vis-1)]

generateState :: Int -> [(Int, Int)] -> State
generateState k ns = ST (DM.fromList $ generateNodeList k ns) False True

generateNodeList :: Int -> [(Int,Int)] -> [(Int, Node)]
generateNodeList _ [] = []
generateNodeList k ((vi,i):nodes) = 
    (i, N i (foldr (\(vi',i') a-> if (abs $ vi - vi') >= k then i':a else a ) 
            [] nodes) Not Not) : generateNodeList k nodes

findPath :: State -> Int -> Int -> State
findPath (ST nodes _ _) f c = foldl (\(ST a stop end) o ->
        let n' = case DM.lookup o a of Just node -> node
                                       _ -> N 0 [] Not Not in 
        if stop then (ST a True end) else
            if (nc $ n') /= (From c) then
                case nv n' of
                    Not -> ST (setnv (From f) o $ setnc (From c) o $ a) True False
                    From o' -> if (not $ send st') then
                                   ST (setnv (From f) o $ sns $ st') True False
                               else
                                   ST (sns $ st') False True
                        where st' = findPath (ST (setnc (From c) o a) False True) o' c
            else
                ST a False end
        ) (ST nodes False True) $ ns $ n
    where n = case DM.lookup f nodes of Just node -> node
                                        _ -> N 0 [] Not Not

setnv :: Visited -> Int -> DM.Map Int Node -> DM.Map Int Node
setnv v i ns = DM.update (\(N o s c _) -> Just $ N o s c v) i ns

setnc :: Visited -> Int -> DM.Map Int Node -> DM.Map Int Node
setnc c i ns = DM.update (\(N o s _ v) -> Just $ N o s c v) i ns

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


