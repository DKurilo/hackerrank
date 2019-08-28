module Main where

import Control.Monad
import System.IO

-- Complete the staircase function below.
staircase :: Int -> [String]
staircase n = foldl (\ss i -> take n (replicate i ' ' ++ repeat '#'): ss)
              [] [0..(n - 1)]

main :: IO()
main = staircase <$> (readLn :: IO Int) >>= mapM_ putStrLn
