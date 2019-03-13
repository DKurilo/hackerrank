{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import System.Environment
import System.IO
import Debug.Trace

-- Complete the superDigit function below.
superDigit ∷ [Int] → Int → Int
superDigit ns k = superDigit' (sum ns * k)

superDigit' ∷ Int → Int
superDigit' n
    | n < 10 = n
    | otherwise = superDigit' ∘ sum ∘ digits $ n

digits ∷ Int → [Int]
digits n
    | n < 10 = [n]
    | otherwise = m:digits d
    where (d,m) = divMod n 10

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    (sn:sk:_) ← BSC.split ' ' <$> BSC.getLine

    let ns = map getInt ∘ BSC.groupBy (\_ _ → False) $ sn

    let k = getInt $ sk

    let result = superDigit ns k

    BSC.putStrLn ∘ BSC.pack ∘ show $ result
    -- BSC.hPutStrLn fptr ∘ BSC.pack ∘ show $ result

    hFlush fptr
    hClose fptr

