module Main (main) where

import Control.Monad (replicateM)
import Data.List (intercalate)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

parseField :: [String] -> [(Int, Int)]
parseField css =
  [ (x, y)
    | y <- [0 .. length css - 1],
      x <- [0 .. length (css !! y) - 1],
      css !! y !! x == 'x'
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  w <- read <$> getLine
  h <- read <$> getLine
  field <- parseField <$> replicateM h getLine

  putStrLn . intercalate "\n" $
    [ [ if n == 0 || (x, y) `elem` field then '.' else (head . show) n
        | x <- [0 .. w - 1],
          let n =
                length
                  [ True
                    | dx <- [- 1 .. 1],
                      let x' = x + dx,
                      dy <- [- 1 .. 1],
                      let y' = y + dy,
                      dx /= 0 || dy /= 0,
                      (x', y') `elem` field
                  ]
      ]
      | y <- [0 .. h - 1]
    ]
