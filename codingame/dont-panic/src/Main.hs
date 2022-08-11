module Main where

import Control.Monad
import Data.List
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  ns <- map read . words <$> getLine
  es <- (<> [ns !! 4]) . map (!! 1) . sort <$> replicateM (ns !! 7) (map read . words <$> getLine) :: IO [Int]
  forever $ do
    (f, p, d) <- (\[f, p, d] -> (read f, read p, d == "LEFT")) . words <$> getLine
    putStrLn $ if f /= -1 && es !! f /= p && d == (es !! f > p) then "BLOCK" else "WAIT"
