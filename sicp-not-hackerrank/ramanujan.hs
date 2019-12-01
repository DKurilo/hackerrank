{-#LANGUAGE OverloadedStrings, TupleSections #-}
-- Some exercises from SICP
module Main where

import Data.Ord
import qualified Data.ByteString.Char8 as B

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x:interleave ys xs

pairs :: [a] -> [b] -> [(a, b)]
pairs [] _ = []
pairs _ [] = []
pairs (x:xs) (y:ys) = (x,y):interleave (map (x,) ys) (pairs xs ys)


mergeWeighted :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeWeighted _ [] ys = ys
mergeWeighted _ xs [] = xs
mergeWeighted w (x:xs) (y:ys)
    | w x < w y = x:mergeWeighted w xs (y:ys)
    | w x > w y = y:mergeWeighted w (x:xs) ys
    | w x == w y = x:y:mergeWeighted w xs ys

pairsWeighted :: Ord c => (a -> b -> c) -> [a] -> [b] -> [(a,b)]
pairsWeighted _ [] _ = []
pairsWeighted _ _ [] = []
pairsWeighted w (x:xs) (y:ys) = (x,y):mergeWeighted w' (map (x,) ys) (pairsWeighted w xs ys)
    where w' = uncurry w

ramanujan :: [Int]
ramanujan = go 0 False (map (uncurry w) (pairsWeighted w [1..] [1..]))
    where w x y = x^3 + y^3
          go _ _ [] = []
          go n r (x:xs)
              | n == x && not r = n:go n True xs
              | n == x && r = go n True xs
              | n /= x = go x False xs

main :: IO ()
main = B.putStrLn . B.intercalate ", " . map (B.pack . show) . take 3000 $ ramanujan
