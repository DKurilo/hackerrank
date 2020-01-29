-- find element that don't have pair in the given list in constant space and O(n) complexity
module Main where

findSingle :: [Int] -> Int
findSingle ns = unadjust . foldl (\p x -> let x' = adjust x in
                                          if p `mod` x' == 0 then p `div` x' else p * x') 1 $ ns
    where bottom = minimum ns
          top = maximum ns
          adjust x = x - bottom + 1 + top
          unadjust x = x + bottom - 1 - top

count :: Int -> [Int] -> Int
count x = foldl (\s n -> (if n == x then 1 else 0) + s) 0

test :: ([Int] -> Int) -> [Int] -> Bool
test f ns = length ns `mod` 2 == 0 || count x ns `mod` 2 == 1
    where x = f ns

main :: IO ()
main = mapM_ (print . test findSingle) [ [1, 9, 9, 1, 12, 12, 5]
                                       , [1, 9, 9, 100, 1, 12, 12]
                                       , [1, 9, 9, 1, 12, 5, 12]
                                       , [1, 9, 9, 1, 12, 50, 12]
                                       , [1, 9, 9, 1, 12, -50, 12]
                                       , [1, 9, 9, 1, 12, 9, 12]
                                       , [1, 9, 9, 1, 12, 12]
                                       , [1, 9]
                                       , [1]
                                       , [0]
                                       , [1, 9, 9, -5, 1, -12, -12]
                                       , []
                                       , [1,2,7,3,0,5,7,1,2,7,3,5,7]
                                       ]
