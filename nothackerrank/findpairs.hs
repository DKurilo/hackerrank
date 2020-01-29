-- find all pairs with sum equal to given value
module Main where

import qualified Data.Set as S

findPairs :: Int -> [Int] -> [(Int, Int)]
findPairs n = fst . foldl (\(ps, ns) x -> let y = n - x
                                              ns' = S.insert x ns in
                                          if y `S.member` ns
                                            then ((y, x):ps, ns')
                                            else (ps, ns')) ([], S.empty)

main :: IO ()
main = print $ findPairs 10 [1, 2, 9, 12, 3, 4, 6, 1, 2, 7, 11, -1, 3432423423423424]
