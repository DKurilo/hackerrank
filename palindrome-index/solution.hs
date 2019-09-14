{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode -- so no we can use ←, ∷, etc
import Control.Monad.Unicode -- so now we can use ≫, ≫=, etc.
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.Environment
import System.IO

palindromeIndex :: String -> Int
palindromeIndex s = index iss
    where s' = reverse s
          iss = [i | (c, c', i) <- zip3 s s' [0..], c /= c']
          index [] = -1
          index ics
              | (check ∘ remove) i1 = i1
              | (check ∘ remove) i2 = i2
              | otherwise = -1
              where i1 = head ics
                    i2 = last ics
          remove i = take i s ++ drop (i + 1) s
          check cs = cs == reverse cs

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    q ← getInt <$> BSC.getLine
    forM_ [1..q] $ \_ →  BSC.pack ∘ show ∘ palindromeIndex ∘ BSC.unpack <$>
    --                      BSC.getLine >>= BSC.hPutStrLn fptr
                         BSC.getLine >>= BSC.putStrLn
    
    hFlush fptr
    hClose fptr
