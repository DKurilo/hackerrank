{-#LANGUAGE OverloadedStrings #-}
-- FizzBuzz
module Main where

import Data.Ord
import qualified Data.ByteString.Char8 as B

-- Fizz in case mod 3, Buzz in case mod 5, FizzBuzz in case both, number for other cases
fizzbuzzTrivialClassic :: [B.ByteString]
fizzbuzzTrivialClassic = [B.concat $ take 2 $ ["Fizz" | n `mod` 3 == 0] ++ ["Buzz" | n `mod` 5 == 0] ++ ["", B.pack . show $ n] | n <- [1..]]

-- Let's slightly change it:
-- Fizz in case mod 3, Buzz in case mod 5, FizzBuzz in case both, number in case mod 7 and not Fizz or Buzz or FizzBuzz
-- then:
-- trivial solution become not the optimal:
fizzbuzzTrivial :: [B.ByteString]
fizzbuzzTrivial = filter (/="") [B.concat $ take 2 $ ["Fizz" | n `mod` 3 == 0] ++ ["Buzz" | n `mod` 5 == 0] ++ [""] ++ [B.pack . show $ n | n `mod` 7 == 0] | n <- [1..]]

-- and it's better to merge arrays:
fizzbuzz :: [B.ByteString]
fizzbuzz = map snd (merge const (merge (\(x,y) (_,y') -> (x, y <> y')) pairs3 pairs5) pairs7)
    where numbs = [1..]
          pairs7 = map (\x -> let x' = x * 7 in (x', B.pack . show $ x')) numbs
          pairs3 = map (\x -> let x' = x * 3 in (x', "Fizz")) numbs
          pairs5 = map (\x -> let x' = x * 5 in (x', "Buzz")) numbs
          merge = mergeAscList (\x -> compare (fst x) . fst)

mergeAscList :: (a -> a -> Ordering) -> (a -> a -> a) -> [a] -> [a] -> [a]
mergeAscList _ _ [] ys = ys
mergeAscList _ _ xs [] = xs
mergeAscList p f (x:xs) (y:ys)
    | p x y == LT = x:mergeAscList p f xs (y:ys)
    | p x y == GT = y:mergeAscList p f (x:xs) ys
    | p x y == EQ = f x y:mergeAscList p f xs ys

main :: IO ()
main = B.putStrLn . B.intercalate "\n" . take 100 . drop 10000000 $ fizzbuzz
