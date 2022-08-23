module Main (main) where

import Data.List (intercalate)
import System.IO

data Grid
  = Grid
      { gTopLeft :: Grid,
        gTopRight :: Grid,
        gBottomLeft :: Grid,
        gBottomRight :: Grid
      }
  | Hole
  | LTrin String

instance Show Grid where
  show g = intercalate "\n" [concat [showEl x y | x <- [0 .. size * 2]] | y <- [0 .. size * 2]]
    where
      showEl x y
        | even x && even y = "+"
        | even x && odd y =
          if lookup (x `div` 2 - 1, (y - 1) `div` 2) cells == lookup (x `div` 2, (y - 1) `div` 2) cells
            then " "
            else "|"
        | odd x && even y =
          if lookup ((x - 1) `div` 2, y `div` 2 - 1) cells == lookup ((x - 1) `div` 2, y `div` 2) cells
            then "  "
            else "--"
        | otherwise =
          if lookup ((x - 1) `div` 2, (y - 1) `div` 2) cells == Just "hole"
            then "##"
            else "  "
      (cells, size) = buildCells g
      buildCells :: Grid -> ([((Int, Int), String)], Int)
      buildCells Hole = ([((0, 0), "hole")], 1)
      buildCells (LTrin cs) = ([((0, 0), cs)], 1)
      buildCells g' = (topLeft <> topRight <> bottomLeft <> bottomRight, n * 2)
        where
          (topLeft, n) = buildCells . gTopLeft $ g'
          topRight = map (\((x, y), cs) -> ((x + n, y), cs)) . fst . buildCells . gTopRight $ g'
          bottomLeft = map (\((x, y), cs) -> ((x, y + n), cs)) . fst . buildCells . gBottomLeft $ g'
          bottomRight = map (\((x, y), cs) -> ((x + n, y + n), cs)) . fst . buildCells . gBottomRight $ g'

buildTopLeftGrid :: Int -> String -> Grid
buildTopLeftGrid 1 path = LTrin path
buildTopLeftGrid n path =
  Grid
    (buildTopLeftGrid n' path')
    (buildTopRightGrid n' path')
    (buildBottomLeftGrid n' path')
    (buildHoledGrid n' (n' - 1) (n' - 1) path' (LTrin path))
  where
    n' = n `div` 2
    path' = '1' : path

buildTopRightGrid :: Int -> String -> Grid
buildTopRightGrid 1 path = LTrin path
buildTopRightGrid n path =
  Grid
    (buildTopLeftGrid n' ('2' : path))
    (buildTopRightGrid n' ('2' : path))
    (buildHoledGrid n' 0 (n' - 1) path' (LTrin path))
    (buildBottomRightGrid n' ('2' : path))
  where
    n' = n `div` 2
    path' = '2' : path

buildBottomLeftGrid :: Int -> String -> Grid
buildBottomLeftGrid 1 path = LTrin path
buildBottomLeftGrid n path =
  Grid
    (buildTopLeftGrid n' ('3' : path))
    (buildHoledGrid n' (n' - 1) 0 path' (LTrin path))
    (buildBottomLeftGrid n' ('3' : path))
    (buildBottomRightGrid n' ('3' : path))
  where
    n' = n `div` 2
    path' = '3' : path

buildBottomRightGrid :: Int -> String -> Grid
buildBottomRightGrid 1 path = LTrin path
buildBottomRightGrid n path =
  Grid
    (buildHoledGrid n' 0 0 path' (LTrin path))
    (buildTopRightGrid n' ('4' : path))
    (buildBottomLeftGrid n' ('4' : path))
    (buildBottomRightGrid n' ('4' : path))
  where
    n' = n `div` 2
    path' = '4' : path

buildHoledGrid :: Int -> Int -> Int -> String -> Grid -> Grid
buildHoledGrid 1 _ _ _ g = g
buildHoledGrid n x y path g = Grid topLeft topRight bottomLeft bottomRight
  where
    n' = n `div` 2
    path' = 'h' : path
    topLeft
      | x < n' && y < n' = buildHoledGrid n' x y path' g
      | otherwise = buildTopLeftGrid n' path'
    topRight
      | x >= n' && y < n' = buildHoledGrid n' (x - n') y path' g
      | otherwise = buildTopRightGrid n' path'
    bottomLeft
      | x < n' && y >= n' = buildHoledGrid n' x (y - n') path' g
      | otherwise = buildBottomLeftGrid n' path'
    bottomRight
      | x >= n' && y >= n' = buildHoledGrid n' (x - n') (y - n') path' g
      | otherwise = buildBottomRightGrid n' path'

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  n <- read <$> getLine :: IO Int
  (x : y : _) <- map read . words <$> getLine
  print $ buildHoledGrid (2 ^ n) x y "" Hole
