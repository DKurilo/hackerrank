module Main (main) where

import Control.Monad (replicateM)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import qualified Text.ParserCombinators.ReadP as RP
import Text.Read

data Arr = Arr {aName :: String, aFirstIndex :: Int, aLastIndex :: Int, aElems :: [Int]} deriving (Show)

instance Read Arr where
  readPrec = lift $ do
    name <- RP.munch1 (/= '[')
    _ <- RP.char '['
    x <- readPrec_to_P readPrec 0
    _ <- RP.string ".."
    y <- readPrec_to_P readPrec 0
    _ <- RP.string "] = "
    xs <- RP.many1 $ do
      x' <- readPrec_to_P readPrec 0
      RP.skipSpaces
      return x'
    return $ Arr name x y xs

data Query = Get String Int | GetNested String Query deriving (Show)

instance Read Query where
  readPrec =
    lift $
      ( do
          name <- RP.munch1 (/= '[')
          _ <- RP.char '['
          index <- readPrec_to_P (readPrec :: ReadPrec Int) 0
          _ <- RP.char ']'
          return $ Get name index
      )
        RP.+++ ( do
                   name <- RP.munch1 (/= '[')
                   _ <- RP.char '['
                   index <- readPrec_to_P (readPrec :: ReadPrec Query) 0
                   _ <- RP.char ']'
                   return $ GetNested name index
               )

execute :: Query -> [Arr] -> Int
execute (Get name i) arrs = aElems arr !! (i - aFirstIndex arr)
  where
    arr = head . filter ((== name) . aName) $ arrs
execute (GetNested name q) arrs = execute (Get name i) arrs
  where
    i = execute q arrs

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  n <- read <$> getLine
  arrays <- map read <$> replicateM n getLine
  query <- read <$> getLine
  print $ execute query arrays
