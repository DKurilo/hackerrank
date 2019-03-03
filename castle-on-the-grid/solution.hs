{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.List as DL
import Data.List.Split
import qualified Data.Set as DS
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the minimumMoves function below.
data Cell = Wall | D Int | Empty
    deriving (Eq)

instance Show Cell where
    show Wall = "X"
    show Empty = "."
    show (D x) = show x

type Grid = Array (Int, Int) Cell

show' g = join $ map (\y → (join $ map (\x → show $ g ! (x,y)) [sw..ew]) ⧺ "\n" ) [sh..eh]
    where ((sw, sh),(ew,eh)) = bounds g
    
minimumMoves ∷ [String] → Int → Int → Int → Int → Cell
minimumMoves grid startX startY goalX goalY =
    (minimumMoves' grid' [(startX, startY)] (goalX,goalY)) ! (goalX,goalY)
    where grid' = array gbnd [((x, y), process x y) | (x,y) ← range gbnd]
          gbnd = ((-1,-1), (len, len))
          len = length grid
          process ∷ Int → Int → Cell
          process x y
              | x < 0 ∨ y < 0 ∨ x ≡ len ∨ y ≡ len = Wall
              | grid !! x !! y ≡ 'X' = Wall
              | x ≡ startX ∧ y ≡ startY = D 0
              | otherwise = Empty

minimumMoves' ∷ Grid → [(Int, Int)] → (Int, Int) → Grid
minimumMoves' grid ps g
    | grid ! g ≠ Empty = grid
    | otherwise = minimumMoves' grid' ps' g
    where (grid', ps') = foldr process (grid, []) ps
          process ∷ (Int, Int) → (Grid, [(Int,Int)]) → (Grid, [(Int, Int)])
          process p (grid'', ps'') = (grid'' // ug, ps'' ⧺ ps''')
              where ps''' = join ∘ map (\f → f grid'' p) $ [l,r,u,d]
                    ug = map (\p' → let c = grid'' ! p' in 
                                    (p', if c ≡ Empty then (D di) else c)) ps'''
                    l = go (\(x,y) → (x - 1, y))
                    r = go (\(x,y) → (x + 1, y))
                    u = go (\(x,y) → (x, y - 1))
                    d = go (\(x,y) → (x, y + 1))
                    go ∷ ((Int, Int) → (Int, Int)) → Grid → (Int, Int) → [(Int, Int)]
                    go f g p' = case g ! p'' of
                        Empty → p'':go f g p''
                        D x → if x >= di then (p'':go f g p'') else []
                        _ → []
                        where p'' = f p'
                    di = case grid ! p of
                        D x → x + 1
                        _ → 10000000  -- error

readMultipleLinesAsStringArray ∷ Int → IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line ← getLine
    rest ← readMultipleLinesAsStringArray $ n - 1
    return $ line : rest

main ∷ IO()
main = do
    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    n ← readLn ∷ IO Int

    grid ← readMultipleLinesAsStringArray n

    startXStartYTemp ← getLine
    let startXStartY = words startXStartYTemp

    let startX = read (startXStartY !! 0) ∷ Int

    let startY = read (startXStartY !! 1) ∷ Int

    let goalX = read (startXStartY !! 2) ∷ Int

    let goalY = read (startXStartY !! 3) ∷ Int

    let result = minimumMoves grid startX startY goalX goalY

    -- hPutStrLn fptr $ show result
    putStrLn $ show result

    hFlush fptr
    hClose fptr

