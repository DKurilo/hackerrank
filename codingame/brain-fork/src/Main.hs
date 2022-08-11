import Control.Monad
import Data.List (minimumBy)
import Debug.Trace (trace)
import System.IO

rocks :: Int
rocks = 30

letters :: Int
letters = 27

data Op = GoRight | GoLeft | Prev | Next | Take | Loop Program

instance Show Op where
  show GoRight = ">"
  show GoLeft = "<"
  show Prev = "-"
  show Next = "+"
  show Take = "."
  show (Loop prog) = "[" <> show prog <> "]"

newtype Program = Program {unProgram :: [Op]}

instance Semigroup Program where
  p1 <> p2 = Program (unProgram p1 <> unProgram p2)

instance Monoid Program where
  mempty = Program []
  mconcat = Program . foldr ((<>) . unProgram) mempty

instance Show Program where
  show = concatMap show . unProgram

data Machine = Machine {mProg :: Program, mRock :: Int, mRocks :: [Int]} deriving (Show)

charToMem :: Int -> Char
charToMem 1 = 'A'
charToMem 2 = 'B'
charToMem 3 = 'C'
charToMem 4 = 'D'
charToMem 5 = 'E'
charToMem 6 = 'F'
charToMem 7 = 'G'
charToMem 8 = 'H'
charToMem 9 = 'I'
charToMem 10 = 'J'
charToMem 11 = 'K'
charToMem 12 = 'L'
charToMem 13 = 'M'
charToMem 14 = 'N'
charToMem 15 = 'O'
charToMem 16 = 'P'
charToMem 17 = 'Q'
charToMem 18 = 'R'
charToMem 19 = 'S'
charToMem 20 = 'T'
charToMem 21 = 'U'
charToMem 22 = 'V'
charToMem 23 = 'W'
charToMem 24 = 'X'
charToMem 25 = 'Y'
charToMem 26 = 'Z'
charToMem _ = ' '

memToChar :: Char -> Int
memToChar 'A' = 1
memToChar 'B' = 2
memToChar 'C' = 3
memToChar 'D' = 4
memToChar 'E' = 5
memToChar 'F' = 6
memToChar 'G' = 7
memToChar 'H' = 8
memToChar 'I' = 9
memToChar 'J' = 10
memToChar 'K' = 11
memToChar 'L' = 12
memToChar 'M' = 13
memToChar 'N' = 14
memToChar 'O' = 15
memToChar 'P' = 16
memToChar 'Q' = 17
memToChar 'R' = 18
memToChar 'S' = 19
memToChar 'T' = 20
memToChar 'U' = 21
memToChar 'V' = 22
memToChar 'W' = 23
memToChar 'X' = 24
memToChar 'Y' = 25
memToChar 'Z' = 26
memToChar _ = 0

goToRock :: Int -> Machine -> Machine
goToRock to m
  | from == to = m
  | from > to =
    m
      { mRock = to,
        mProg =
          mProg m
            <> Program
              ( if from - to < rocks - from + to
                  then replicate (from - to) GoLeft
                  else replicate (rocks - from + to) GoRight
              )
      }
  | otherwise =
    m
      { mRock = to,
        mProg =
          mProg m
            <> Program
              ( if to - from < rocks + from - to
                  then replicate (to - from) GoRight
                  else replicate (rocks + from - to) GoLeft
              )
      }
  where
    from = mRock m

takeLetter :: Int -> Machine -> Machine
takeLetter l m
  | from == to = m {mProg = mProg m <> Program [Take]}
  | from > to =
    m
      { mRocks = newRocks,
        mProg =
          mProg m
            <> Program
              ( if from - to < letters - from + to
                  then replicate (from - to) Prev
                  else replicate (letters - from + to) Next
              )
            <> Program [Take]
      }
  | otherwise =
    m
      { mRocks = newRocks,
        mProg =
          mProg m
            <> Program
              ( if to - from < letters + from - to
                  then replicate (to - from) Next
                  else replicate (letters + from - to) Prev
              )
            <> Program [Take]
      }
  where
    from = mRocks m !! mRock m
    to = l
    currRocks = mRocks m
    newRocks = take (mRock m) currRocks <> (l : drop (mRock m + 1) currRocks)

addLetter :: Int -> Machine -> Machine
addLetter l =
  minimumBy (\m1 m2 -> compare (progLength m1) (progLength m2))
    . zipWith (\i m -> takeLetter l . goToRock i $ m) [0 .. rocks - 1]
    . repeat
  where
    progLength = length . unProgram . mProg

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  -- Auto-generated code below aims at helping you parse
  -- the standard input according to the problem statement.

  magicphrase <- map memToChar <$> getLine

  -- hPutStrLn stderr "Debug messages..."

  -- Write action to stdout
  print . mProg . foldl (flip addLetter) (Machine (Program []) 0 (replicate rocks 0)) $ magicphrase
  return ()
