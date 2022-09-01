module Main (main) where

import Control.Monad
import Data.Bifunctor (Bifunctor (second))
import Data.List (intercalate, minimumBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import System.IO
import qualified Text.ParserCombinators.ReadP as RP
import Text.Read

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

data Tile = TUp | TDown | TLeft | TRight | TTreasure deriving (Show)

instance Read Tile where
  readPrec =
    lift $
      (RP.char '^' >> return TUp)
        RP.+++ (RP.char 'v' >> return TDown)
        RP.+++ (RP.char '<' >> return TLeft)
        RP.+++ (RP.char '>' >> return TRight)
        RP.+++ (RP.char 'T' >> return TTreasure)

newtype Treasure = Treasure {unTreasure :: Map Point Tile} deriving (Show)

parseTreasureMap :: [String] -> Treasure
parseTreasureMap =
  Treasure
    . M.fromList
    . map (second (fromMaybe TUp))
    . filter (isJust . snd)
    . join
    . zipWith (\y -> zipWith (\x c -> (Point x y, readMaybe [c])) [0 ..]) [0 ..]

pathLength :: Point -> Treasure -> Maybe Int
pathLength = pathLength' S.empty

pathLength' :: Set Point -> Point -> Treasure -> Maybe Int
pathLength' visited p t
  | p `S.member` visited = Nothing
  | otherwise = case p `M.lookup` unTreasure t of
    Just TTreasure -> Just 1
    Just TUp -> (+ 1) <$> pathLength' visited' (p {pY = pY p - 1}) t
    Just TDown -> (+ 1) <$> pathLength' visited' (p {pY = pY p + 1}) t
    Just TLeft -> (+ 1) <$> pathLength' visited' (p {pX = pX p - 1}) t
    Just TRight -> (+ 1) <$> pathLength' visited' (p {pX = pX p + 1}) t
    _ -> Nothing
  where
    visited' = p `S.insert` visited

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (_ : h : _) <- map read . words <$> getLine
  start <-
    (\ns -> Point (last ns) (head ns))
      . map read
      . words
      <$> getLine
  n <- read <$> getLine
  treasureMaps <- map parseTreasureMap <$> replicateM n (replicateM h getLine)
  let pathes = zipWith (\i t -> (i, pathLength start t)) [(0 :: Int) ..] treasureMaps
  putStrLn $
    if all (isNothing . snd) pathes
      then "TRAP"
      else
        show
          . fst
          . minimumBy
            ( \x y -> case snd x `compare` snd y of
                EQ -> fst x `compare` fst y
                r -> r
            )
          . filter (isJust . snd)
          $ pathes
