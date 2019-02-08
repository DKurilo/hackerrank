-- FizzBuzz
module Main where

import Control.Monad

main :: IO ()
main = do
    forM_ [1..100] $ \n -> do
        putStrLn $ concat $ take 2 $ ["Fizz" | n `mod` 3 == 0] ++ ["Buzz" | n `mod` 5 == 0] ++ ["", show n]

