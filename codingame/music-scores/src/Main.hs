module Main where

import Control.Monad
import Data.List (break, intercalate, splitAt, transpose)
import Data.Vector (Vector (..))
import qualified Data.Vector as V
import Debug.Trace (trace)
import System.IO
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Pixel = B | W deriving (Eq, Ord)

instance Show Pixel where
  show B = "#"
  show W = "."

instance Read Pixel where
  readPrec = ((lift . P.char $ 'W') >> return W) PC.+++ ((lift . P.char $ 'B') >> return B)

newtype Image = Image {unImage :: Vector (Vector Pixel)} deriving (Eq, Ord)

instance Show Image where
  show = intercalate "\n" . map (concatMap show . V.toList) . V.toList . unImage

data Lines = Lines {lStart :: Int, lWidth :: Int, lDistance :: Int} deriving (Show)

data Tone = TA | TB | TC | TD | TE | TF | TG

instance Show Tone where
  show TA = "A"
  show TB = "B"
  show TC = "C"
  show TD = "D"
  show TE = "E"
  show TF = "F"
  show TG = "G"

numberToTone :: Int -> Tone
numberToTone n = [TA, TB, TC, TD, TE, TF, TG] !! ((21 - 1 - n) `mod` 7)

data Beat = H | Q deriving (Eq, Show)

data Note = Note {nTone :: Tone, nBeat :: Beat}

instance Show Note where
  show (Note t b) = show t <> show b

newtype MusicScores = MusicScores {unMusicScores :: [Note]}

instance Show MusicScores where
  show = unwords . map show . unMusicScores

unFlat :: Int -> [a] -> [[a]]
unFlat n xs = case rest of
  [] -> [chunk]
  _ -> chunk : unFlat n rest
  where
    (chunk, rest) = splitAt n xs

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s = case rest of
  [] -> [chunk]
  _ : rest -> chunk : splitOn p rest
  where
    (chunk, rest) = break p s

split :: Char -> String -> [String]
split c = splitOn (== c)

sep :: [String] -> ([String], [String])
sep [] = ([], [])
sep (cs1 : cs2 : css) = (cs1 : cs1s, cs2 : cs2s)
  where
    (cs1s, cs2s) = sep css

parseImage :: Int -> String -> Image
parseImage w cs = Image (V.fromList . map V.fromList $ imageData)
  where
    chunks = split ' ' cs
    (pixStrs, lengthsStrs) = sep chunks
    pixels :: [Pixel]
    pixels = map read pixStrs
    lengths :: [Int]
    lengths = map read lengthsStrs
    imageData = unFlat w . concat $ zipWith replicate lengths pixels

detectLines :: Image -> Lines
detectLines (Image image) = Lines firstLineN firstLineW (secondLineN - firstLineN)
  where
    h = V.length image `div` 3
    column =
      fst
        . head
        . dropWhile (not . snd)
        . zip [0 ..]
        . map (elem B)
        . transpose
        . V.toList
        . V.map V.toList
        . V.take h
        $ image
    line = [image V.! i V.! column | i <- [0 .. V.length image - 1]]
    firstLineN = length . takeWhile (== W) $ line
    firstLineW = length . takeWhile (== B) . drop firstLineN $ line
    secondLineN = firstLineN + firstLineW + (length . takeWhile (== W) . drop (firstLineN + firstLineW)) line

isOnLine :: Lines -> Int -> Bool
isOnLine ls i = (i - lStart ls) `mod` lDistance ls < lWidth ls

notesHist :: Lines -> Image -> [(Int, Beat)]
notesHist lines image = map (\(n, cnt) -> (n, if cnt > threshold then Q else H)) segments
  where
    isLine = isOnLine lines
    imageHeight = V.length . unImage $ image
    imageWidth = V.length . (V.! 0) . unImage $ image
    hist =
      map
        (\i -> (i, length . filter (== B) $ [(V.! i) . (V.! k) . unImage $ image | k <- [0 .. imageHeight - 1], (not . isLine) k]))
        [0 .. imageWidth - 1]
    extendedSegments =
      map
        ( \cs ->
            let maxN = fst . last $ cs
                minN = fst . head $ cs
             in ((maxN + minN) `div` 2, sum . map snd $ cs, maxN - minN)
        )
        . filter (not . null)
        . splitOn ((== 0) . snd)
        $ hist
    segments = map (\(i, cnt, _) -> (i, cnt)) extendedSegments
    noteWidth = (\(_, _, w) -> w) . head $ extendedSegments
    -- pi * d * d / 4 + area of stick
    threshold = noteWidth * (lDistance lines - lWidth lines) * 3 `div` 4 + lDistance lines

detectNote :: Image -> Lines -> Int -> Tone
detectNote image lines i = numberToTone noteNumber
  where
    imageHeight = V.length . unImage $ image
    firstBlack =
      fst
        . head
        . dropWhile ((== W) . snd)
        $ [(k, (V.! i) . (V.! k) . unImage $ image) | k <- [0 .. imageHeight - 1], (not . isOnLine lines) k]
    noteNumber = (firstBlack - lStart lines + lDistance lines) * 2 `div` lDistance lines

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE
  input <- words <$> getLine
  let width = read (head input) :: Int
  let height = read (input !! 1) :: Int
  image <- parseImage width <$> getLine
  let lines = detectLines image
      hist = notesHist lines image
      musicScores = MusicScores . map (\(pos, b) -> Note (detectNote image lines pos) b) $ hist
  print musicScores
