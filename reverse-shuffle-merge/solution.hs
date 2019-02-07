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

data CSt = New|First|Second|Removed deriving (Eq, Ord, Show)
type PChar = (CSt, Char)

reverseShuffleMerge :: String -> String
reverseShuffleMerge s = reverseShuffleMerge' (reverse s) $ cset . DL.sort $ s

reverseShuffleMerge' :: String -> [PChar] -> String
reverseShuffleMerge' "" _ = ""
reverseShuffleMerge' _ [] = ""
reverseShuffleMerge' cs ps = cs''' ++ [c| c /= '0'] ++ reverseShuffleMerge' cs'' ps''
    where (cs', cs'', c, ps') = breakInTwo cs [] ps
          (cs''',ps'',_,_) = foldr (\c' (a, pcs, c'', r) ->
                 if not r && c''==c && c' == c then (a,pcs,c'',True)
                 else 
                     if c' <= c'' then
                         (c':a, removeFirstPc c' pcs, c',r) 
                     else (a,pcs,c'',r))
                 ([],ps',c,False) $ cs'

removeFirstPc :: Char -> [PChar] -> [PChar]
removeFirstPc _ [] = []
removeFirstPc c ((pst, c'):pcs)
    | c==c' && pst /= Removed = (Removed, c') : pcs
    | otherwise = (pst, c') : removeFirstPc c pcs

breakInTwo :: String -> String -> [PChar] -> (String, String, Char, [PChar])
breakInTwo [] ocs ps = (reverse ocs, "", '0', ps)
breakInTwo (c:cs) ocs ps
        | (filter (\(pst, pc) -> pc==c && pst /= Removed) ps) == [] = breakInTwo cs ocs ps
        | pst == Second = (reverse ocs, cs, c, ps')
        | pst == Removed = breakInTwo cs (c:ocs) ps'
        | otherwise = breakInTwo cs (c:ocs) ps'
    where ((pst, c'), ps') = placePChar c ps []

placePChar :: Char -> [PChar] -> [PChar] -> (PChar, [PChar])
placePChar c ((pst, pc):pcs) opcs
    | pc==c && pst==Removed = ((Removed,pc),reverse opcs ++ pcs)
    | pc==c && pst==New = ((First,pc),reverse opcs ++ ((First,pc):pcs))
    | pc==c && pst==First && (pcs==[] || (snd . head $ pcs) /= c) = 
        ((Second,pc),(reverse opcs) ++ pcs)
    | otherwise = placePChar c pcs ((pst,pc):opcs)

cset :: String -> [PChar]
cset [] = []
cset (c:[]) = []
cset (c:_:cs) = ((New, c):cset cs)

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

