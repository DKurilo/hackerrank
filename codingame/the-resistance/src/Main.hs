import Control.Monad
import Control.Monad.ST (runST)
import Data.List (foldl')
import Data.Map (Map (..))
import qualified Data.Map as M
import Data.Vector (Vector (..))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Debug.Trace (trace)
import System.IO

data Morse = Dot | Dash deriving (Eq, Show)

data MorseTrie
  = Tip
  | Node {mtrieDot :: MorseTrie, mtrieDash :: MorseTrie, mtrieLength :: Int, mtrieCount :: Int}
  deriving (Show)

initTrie :: MorseTrie
initTrie = Tip

addWordToTrie :: [Morse] -> MorseTrie -> MorseTrie
addWordToTrie dds = doer dds
  where
    lengthDds = length dds
    doer :: [Morse] -> MorseTrie -> MorseTrie
    doer [] Tip = Node Tip Tip lengthDds 1
    doer [] tr = tr {mtrieLength = lengthDds, mtrieCount = mtrieCount tr + 1}
    doer (Dot : ds) Tip = Node (doer ds Tip) Tip 0 0
    doer (Dot : ds) tr = tr {mtrieDot = doer ds . mtrieDot $ tr}
    doer (Dash : ds) Tip = Node Tip (doer ds Tip) 0 0
    doer (Dash : ds) tr = tr {mtrieDash = doer ds . mtrieDash $ tr}

charToMorse :: Char -> Morse
charToMorse '.' = Dot
charToMorse _ = Dash

letterToMorse :: Char -> [Morse]
letterToMorse 'A' = [Dot, Dash]
letterToMorse 'B' = [Dash, Dot, Dot, Dot]
letterToMorse 'C' = [Dash, Dot, Dash, Dot]
letterToMorse 'D' = [Dash, Dot, Dot]
letterToMorse 'E' = [Dot]
letterToMorse 'F' = [Dot, Dot, Dash, Dot]
letterToMorse 'G' = [Dash, Dash, Dot]
letterToMorse 'H' = [Dot, Dot, Dot, Dot]
letterToMorse 'I' = [Dot, Dot]
letterToMorse 'J' = [Dot, Dash, Dash, Dash]
letterToMorse 'K' = [Dash, Dot, Dash]
letterToMorse 'L' = [Dot, Dash, Dot, Dot]
letterToMorse 'M' = [Dash, Dash]
letterToMorse 'N' = [Dash, Dot]
letterToMorse 'O' = [Dash, Dash, Dash]
letterToMorse 'P' = [Dot, Dash, Dash, Dot]
letterToMorse 'Q' = [Dash, Dash, Dot, Dash]
letterToMorse 'R' = [Dot, Dash, Dot]
letterToMorse 'S' = [Dot, Dot, Dot]
letterToMorse 'T' = [Dash]
letterToMorse 'U' = [Dot, Dot, Dash]
letterToMorse 'V' = [Dot, Dot, Dot, Dash]
letterToMorse 'W' = [Dot, Dash, Dash]
letterToMorse 'X' = [Dash, Dot, Dot, Dash]
letterToMorse 'Y' = [Dash, Dot, Dash, Dash]
letterToMorse 'Z' = [Dash, Dash, Dot, Dot]
letterToMorse _ = []

mkMorseTrie :: [[Morse]] -> MorseTrie
mkMorseTrie = foldl' (flip addWordToTrie) initTrie

findWordsFromIndex :: Int -> Vector Morse -> MorseTrie -> Map Int Int
findWordsFromIndex i line = doer i
  where
    doer :: Int -> MorseTrie -> Map Int Int
    doer _ Tip = M.empty
    doer i' tr
      | i' < 0 || i' >= V.length line = M.empty
      | line V.! i' == Dot = case dotTr of
        Tip -> M.empty
        tr
          | mtrieCount tr == 0 -> doer (i' + 1) tr
          | otherwise -> M.unionWith (+) (M.singleton (mtrieLength dotTr) (mtrieCount dotTr)) (doer (i' + 1) dotTr)
      | line V.! i' == Dash = case dashTr of
        Tip -> M.empty
        tr
          | mtrieCount dashTr == 0 -> doer (i' + 1) dashTr
          | otherwise -> M.unionWith (+) (M.singleton (mtrieLength dashTr) (mtrieCount dashTr)) (doer (i' + 1) dashTr)
      | otherwise = error "WTF is going on?"
      where
        dotTr = mtrieDot tr
        dashTr = mtrieDash tr

findWords :: MorseTrie -> Vector Morse -> Map Int [(Int, Int)]
findWords tr line =
  foldl'
    ( \mws i ->
        let ws = M.toList . findWordsFromIndex i line $ tr
         in if null ws then mws else M.insert i ws mws
    )
    M.empty
    [0 .. V.length line - 1]

countPossible :: Int -> Map Int [(Int, Int)] -> Int
countPossible lineLength mIndices = doer 0
  where
    doer = (map counts [0 .. lineLength] !!)
    counts :: Int -> Int
    counts start
      | start == lineLength = 1
      | otherwise = case start `M.lookup` mIndices of
        Just is -> sum . map (\(l, n) -> n * doer (start + l)) $ is
        Nothing -> 0

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  morseLine <- V.fromList . map charToMorse <$> getLine
  n <- read <$> getLine
  dict <- mkMorseTrie . map (concatMap letterToMorse) <$> replicateM n getLine
  print . countPossible (V.length morseLine) . findWords dict $ morseLine
  return ()
