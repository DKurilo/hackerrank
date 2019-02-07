{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.List as DL
import Data.List.Split
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the reverseShuffleMerge function below.

data PChar = PC Char Int Int Int
    deriving (Show)

reverseShuffleMerge :: String -> String
reverseShuffleMerge s = reverseShuffleMerge' (reverse s) $ cset s

reverseShuffleMerge' :: String -> [PChar] -> String
reverseShuffleMerge' [] _ = ""
reverseShuffleMerge' (c:[]) ps = [c | (countUsePc c ps) > 0]
reverseShuffleMerge' cs ps = trace (show cs1 ++ " " ++ show c ++ " " ++ show cs2 ++ " " ++ show ps'') $ cs1' ++ reverseShuffleMerge' cs2' ps''
    where (cs1, c, cs2, ps') = breakInTwo cs [] ps
          (cs1',cs2', ps'') = getMinimal cs1 cs2 ps'
--           (cs1',cs2', ps'') = oneStepBack cs1 c cs2 ps'

getMinimal :: String -> String -> [PChar] -> (String, String, [PChar])
getMinimal cs1 cs2 ps
    | cs1f == [] = ("", cs2, ps)
    | otherwise =  ([c], reverse b ++ cs2, ps')
    where cs1f = filter (\c' -> (countUsePc c' ps) > 0) cs1
          c = minimum cs1f
          (b,_) = foldl (\(s,fl) c' -> 
                          if fl then (c':s,fl)
                          else if c'==c then (s,True)
                          else (s,fl)
                      ) ([],False) cs1
          ps' = foldr unmeetPc (usePc c ps) b

oneStepBack :: String -> Char -> String -> [PChar] -> (String, String, [PChar])
oneStepBack cs1 c cs2 ps = (\(s1,s2,p,_,_) -> (s1,s2,p)) $ 
                           foldr perform ([], cs2, ps, [], c) cs1
    where perform c' (s1, s2, p, b, c'')
              | c' == c && c'' == c && (countUsePc c' p) >= 0 =
                 ([c'], b++s2, usePc c' (foldr unusePc (foldr unmeetPc p b) s1),[c'], c)
              | c' <= c'' && (countUsePc c' p) > 0 = (c':s1, s2, usePc c' p, c':b, c')
              | otherwise = (s1, s2, p, c':b, c'')

countUsePc :: Char -> [PChar] -> Int
countUsePc c = (\(PC _ s u m) -> 
                   (s `div` 2) - u) . head . filter (\(PC c' _ _ _) -> c'==c)

usePc :: Char -> [PChar] -> [PChar]
usePc c ((PC c' s u m):pcs)
    | c'==c = (PC c' s (u+1) m): pcs
    | otherwise = (PC c' s u m): usePc c pcs

unusePc :: Char -> [PChar] -> [PChar]
unusePc c ((PC c' s u m):pcs)
    | c'==c = (PC c' s (u-1) m): pcs
    | otherwise = (PC c' s u m): unusePc c pcs

countMeetPc :: Char -> [PChar] -> Int
countMeetPc c = (\(PC _ s u m) -> (s `div` 2) - m + u) . head . 
                filter (\(PC c' _ _ _) -> c'==c)

meetPc :: Char -> [PChar] -> [PChar]
meetPc c ((PC c' s u m):pcs)
    | c'==c = (PC c' s u (m+1)): pcs
    | otherwise = (PC c' s u m): meetPc c pcs

unmeetPc :: Char -> [PChar] -> [PChar]
unmeetPc c ((PC c' s u m):pcs)
    | c'==c = (PC c' s u (m-1)): pcs
    | otherwise = (PC c' s u m): unmeetPc c pcs

breakInTwo :: String -> String -> [PChar] -> (String, Char, String,[PChar])
breakInTwo [] ocs pcs = (reverse ocs, '0', "", pcs)
breakInTwo (c:cs) ocs  pcs
    | m == 0 = (reverse (c:ocs), c, cs, pcs')
    | cs == [] = (reverse (c:ocs), '0', cs, pcs')
    | otherwise = breakInTwo cs (c:ocs) pcs'
    where pcs' = meetPc c pcs
          m = countMeetPc c pcs

cset :: String -> [PChar]
cset cs = filter (\(PC _ x _ _) -> x > 0) $
          map (\c -> PC c (length $ filter (==c) cs) 0 0) ['a'..'z']

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    let result = reverseShuffleMerge s

    -- hPutStrLn fptr result
    putStrLn result

    hFlush fptr
    hClose fptr


