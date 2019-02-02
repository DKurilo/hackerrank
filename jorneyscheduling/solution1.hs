{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import qualified Data.List as DL
import Data.List.Split
import qualified  Data.Set as DS
import System.Environment
import System.IO
import qualified Data.Map.Strict as DM

import Debug.Trace

--
-- Complete the journeyScheduling function below.
--

type Distance = Maybe Int

data FarCity = FC { fcn :: Int    -- number
                  , fcd :: Int    -- distance
                  } deriving (Eq, Ord)

data City = C { cn :: Int         -- number
              , crs :: [Int]      -- available cities
              , cd :: Distance    -- current distance or distance to the farthest city
              } deriving (Eq, Ord)

type Map = DM.Map Int City

data State = ST { stlongest :: Int
                , stfarthest :: Int
                , stmap :: Map
                }

journeyScheduling :: [[Int]] -> Int -> Handle -> IO ()
journeyScheduling roads j fptr = do
    mapM_ (\_ -> do
        line <- getLine
        let (from:hops:_) = DL.map (read :: String -> Int) . words $ line
        getDistance treeland from hops fptr) [1..j]
    where treeland = buildMap roads

buildMap :: [[Int]] -> State
buildMap routes = ST l 0 $! fillFarthest path m
    where n = length routes
          (ST _ _ cities) = fillRoutes routes $! ST 0 0 $ DM.fromList $ 
              map (\i -> (i, C i [] Nothing)) [1..(n+1)]
          from = stfarthest $ findFarthest (ST 0 0 $ cities) 1
          (ST l to m') = findFarthest (ST 0 0 $ cities) $! from
          m = markPath m' to l cities
          path = map fst $ DM.toList $ 
              DM.filter (\(C n _ md) -> case md of Just _ -> True
                                                   _ -> False ) m

fillRoutes :: [[Int]] -> State -> State
fillRoutes [] (ST l _ m) = ST l 0 m
fillRoutes ((c1:c2:_):rs) (ST l _ m) = fillRoutes rs $! ST l 0 $! DM.update (addr c2) c1 $ DM.update (addr c1) c2 $ m
    where addr = \c (C n rs d) -> Just $ C n (c:rs) d
fillRoutes (_:rs) (ST l _ m) = fillRoutes rs $ ST l 0 m

findFarthest :: State -> Int -> State
findFarthest (ST l f clm) i = case DM.lookup i clm of
    Nothing -> ST l f clm
    Just (C n rs d) -> ST (fcd fc) (fcn fc) $! m'
        where (ST l' _ m') = flood [i] $ ST 0 0 clm
              fc = c2fc $ snd $ head $ filter (\(_,(C n'' _ d'')) -> (Just l') == d'') $ DM.toList $ m'

markPath :: Map -> Int -> Int -> Map -> Map
markPath flm s l clm = DM.unionWith (\(C n rs _) (C _ _ d) -> C n rs d) clm $ path 
    where k = \t i -> if i < (t `div` 2 + t `mod` 2) then t - i else i
          path = DM.fromList $ markPath' flm s (l-1) 0 (k l) []

markPath' :: Map -> Int -> Int -> Int -> (Int -> Int) -> [(Int,City)] -> [(Int,City)]
markPath' flm s l c k cs = if l>c then markPath' flm s' l (c+1) k cs' else cs'
    where cs' = (s, C s [] $ Just $ k c):cs
          s' = getCityOnPath s (l-c) $ foldr (\i as-> case DM.lookup i flm of Just city -> city:as
                                                                              _ -> as ) [] $ 
              case DM.lookup s flm of Just (C _ rs _) -> rs
                                      _ -> []

getCityOnPath :: Int -> Int -> [City] -> Int
getCityOnPath _ _ [] = 0 -- error
getCityOnPath i d (C n rs md:cs) | (Just d) == md = n
                                 | otherwise = getCityOnPath i d cs 

fillFarthest :: [Int] -> Map -> Map
fillFarthest [] m = m
fillFarthest (c:cs) m = case DM.lookup c m of 
    Just (C n rs (Just d)) -> fillFarthest rs' m' 
        where (rs', m') = foldr (\r (rs'',m'') ->
                    case DM.lookup r m'' of
                        Just (C n rs Nothing) -> (r:rs'',id $! DM.update (\(C n' rs' _) -> Just $ C n' rs' $ Just $ d+1) n m'')
                        _ -> (rs'',m'')
                ) (cs,m) rs
    _ -> fillFarthest cs m

getRoutes :: Map -> Int -> [Int]
getRoutes m i = case DM.lookup i m of Just (C _ rs _) -> foldr (\j rs'-> case DM.lookup j m of Just (C n _ Nothing) -> (n:rs')
                                                                                               _ -> rs') [] rs
                                      _ -> []

c2fc :: City -> FarCity
c2fc (C n _ (Just d)) = FC n d
c2fc _ = FC 0 0

flood :: [Int] -> State -> State
flood [] (ST l _ m) = ST (l-1) 0 m
flood ns (ST l _ m) = flood ns' (ST (l'+1) 0 m')
    where (ns', ST l' _ m') = foldr (\i (ns'',ST l'' 0 m'') -> 
            case DM.lookup i m'' of
                Just (C n rs d) -> if d /= Nothing then (ns'', ST l'' 0 m'') else 
                        (foldr (\r as -> if r==i then as else case DM.lookup r m'' of 
                                                                  Just (C _ _ Nothing) -> r:as
                                                                  _ -> as) ns'' rs, ST l 0 $ 
                            DM.update (\(C n' rs' d') -> 
                                Just $ C n' rs' (Just l)) n m'')
                _ -> (ns'', ST l'' 0 m'')
            ) ([], ST l 0 m) ns
 
getDistance :: State -> Int -> Int -> Handle -> IO ()
getDistance (ST l _ m) f h fptr = do
    case DM.lookup f m of 
        Just (C _ _ md) -> case md of 
                Just d -> hPutStrLn fptr $ show $ l * (h - 1) + d
                _ -> return () -- error
        _ -> return () -- error
    --
    -- Write your code here.
    --

readMultipleLines :: Int -> IO [[Int]]
readMultipleLines 0 = return []
readMultipleLines n = do
    line <- getLine
    let ns = DL.map (read :: String -> Int) . words $ line
    rest <- readMultipleLines(n - 1)
    return (ns: rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nmTemp <- getLine
    let nm = words nmTemp

    let n = read (nm !! 0) :: Int

    let m = read (nm !! 1) :: Int

    roads <- readMultipleLines $ n-1

    -- journeys <- readMultipleLines m

    journeyScheduling roads m fptr

    -- hPutStrLn fptr $ DL.intercalate "\n" $ DL.map (\x -> show x) $ result
    -- putStrLn $ DL.intercalate "\n" $ DL.map (\x -> show x) $ result
    hFlush fptr
    hClose fptr

