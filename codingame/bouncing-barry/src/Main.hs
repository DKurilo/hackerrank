import Control.Monad
import Data.Bifunctor (bimap)
import Data.List (foldl', intercalate)
import Data.Set (Set (..))
import qualified Data.Set as S
import System.IO

data Coord = Coord {cX :: Int, cY :: Int} deriving (Eq, Ord)

data Direction = N | S | E | W deriving (Show)

charToDirection :: Char -> Direction
charToDirection 'N' = N
charToDirection 'S' = S
charToDirection 'E' = E
charToDirection _ = W

data Jump = Jump {jDir :: Direction, jLength :: Int} deriving (Show)

newtype Grid = Grid {unGrid :: Set Coord}

instance Show Grid where
  show (Grid grid)
    | S.null grid = "."
    | otherwise = intercalate "\n" [[if Coord x y `S.member` grid then '#' else '.' | x <- [xMin .. xMax]] | y <- [yMin .. yMax]]
    where
      xs = S.map cX grid
      ys = S.map cY grid
      xMin = S.findMin xs
      xMax = S.findMax xs
      yMin = S.findMin ys
      yMax = S.findMax ys

mkGrid :: [Jump] -> Grid
mkGrid = snd . foldl' (\(c, g) j -> doJump j c g) (Coord 0 0, Grid S.empty)
  where
    doJump :: Jump -> Coord -> Grid -> (Coord, Grid)
    doJump j c g =
      ( Coord (cX c + dx * jLength j) (cY c + dy * jLength j),
        Grid $ foldl' (\g' c' -> if c' `S.member` g' then S.delete c' g' else S.insert c' g') (unGrid g) cs
      )
      where
        (dx, dy) = case jDir j of
          N -> (0, -1)
          S -> (0, 1)
          E -> (1, 0)
          W -> (-1, 0)
        cs = map (\n -> Coord (cX c + dx * n) (cY c + dy * n)) [1 .. jLength j]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  -- Auto-generated code below aims at helping you parse
  -- the standard input according to the problem statement.

  directions <- words <$> getLine
  bounces <- words <$> getLine

  let jumps = zipWith (curry (uncurry Jump . bimap (charToDirection . head) read)) directions bounces

  -- hPutStrLn stderr "Debug messages..."

  -- Write answer to stdout
  print . mkGrid $ jumps
  return ()
