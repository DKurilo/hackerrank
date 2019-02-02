module Main where

import System.IO

data Canvas = C { cw :: Int
                , ch :: Int
                , ce :: Char
                , cf :: Char
                }

data Point = O|X

-- settings
bh :: Int
bh = 32
cnv :: Canvas
cnv = C 180 127 '_' '1'

drawAll :: Int -> Int -> [[Point]]
drawAll h 1 = drawY h
drawAll h n = (join2 (h*2) $ drawAll (h `div` 2) (n-1)) ++ drawY h

join2 :: Int -> [[Point]] -> [[Point]]
join2 n pss = map (\ps -> ps ++ (take (n-1-((length ps) `div` 2)*2) $ repeat O) ++ ps) pss

drawY :: Int -> [[Point]]
drawY n = (drawV n) ++ (take n $ repeat [X])

drawV :: Int -> [[Point]]
drawV 0 = [[X,X]]
drawV 1 = [[X,O,X]]
drawV n = [[X] ++ (take (n*2-1) $ repeat O) ++ [X]] ++ (drawV (n-1))

placeOnCanvas :: Canvas -> [[Point]] -> [String]
placeOnCanvas (C w h e f) pss = (take (h - length pss) $ repeat $ emptyLine w) ++ 
    map (\ps -> center $ map draw ps) pss
    where hw = w `div` 2
          emptyLine n = take n $ repeat e
          center cs = let csl = length cs in 
                      emptyLine (hw-(csl `mod` 2)-(csl `div` 2)) ++ cs ++ emptyLine (hw-csl `div` 2)
          draw O = e
          draw X = f

main :: IO ()
main = do
    sn <- getLine
    let n = read sn :: Int
    mapM_ putStrLn $ placeOnCanvas cnv $ drawAll bh n

