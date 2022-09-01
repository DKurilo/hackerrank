module Main (main) where

import System.IO

allSizes :: [Int] -> [Int]
allSizes xs
  | l < 2 = []
  | otherwise = [xs !! j - xs !! i | i <- [0 .. l - 2], j <- [i + 1 .. l - 1]]
  where
    l = length xs

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  (w : h : _) <- map read . words <$> getLine
  xs <- map read . words <$> getLine
  ys <- map read . words <$> getLine

  let xs' = [0] <> xs <> [w]
      ys' = [0] <> ys <> [h]
      ws = allSizes xs'
      hs = allSizes ys'

  print . length $ [True | i <- ws, j <- hs, i == j]
