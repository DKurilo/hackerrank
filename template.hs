{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.Environment
import System.IO

-- function is here

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    -- read one int
    n ← getInt <$> BSC.getLine
    -- read line of ints (1 2 3 5)
    as ← getInts <$> BSC.getLine

    m ← getInt <$> BSC.getLine
    -- read multi lines with one int in each.
    qs ← forM [1..m] $ \_ → getInt <$> BSC.getLine

    let ans =  1 -- your function call

    BSC.putStrLn $ BSC.intercalate "\n" $ map (BSC.pack ∘ show) ans
    -- BSC.hPutStrLn fptr $ BSC.intercalate "\n" $ map (BSC.pack ∘ show) ans

    hFlush fptr
    hClose fptr

