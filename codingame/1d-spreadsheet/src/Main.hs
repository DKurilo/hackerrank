module Main (main) where

import Control.Monad
import Data.Maybe (fromMaybe)
import System.IO
import Text.ParserCombinators.ReadP as R
import Text.ParserCombinators.ReadPrec as RP
import Text.Read

data Val = Ref Int | Val Int deriving (Show)

instance Read Val where
  readPrec =
    ( do
        _ <- lift . R.char $ '$'
        Ref <$> readPrec
    )
      RP.<++ (Val <$> readPrec)

data Op = Value Val | Add Val Val | Sub Val Val | Mul Val Val deriving (Show)

instance Read Op where
  readPrec =
    ( do
        _ <- lift . R.string $ "VALUE"
        lift R.skipSpaces
        val <- readPrec
        lift R.skipSpaces
        _ <- lift . R.char $ '_'
        return . Value $ val
    )
      RP.+++ ( do
                 _ <- lift . R.string $ "ADD"
                 lift R.skipSpaces
                 val1 <- readPrec
                 lift R.skipSpaces
                 Add val1 <$> readPrec
             )
      RP.+++ ( do
                 _ <- lift . R.string $ "SUB"
                 lift R.skipSpaces
                 val1 <- readPrec
                 lift R.skipSpaces
                 Sub val1 <$> readPrec
             )
      RP.+++ ( do
                 _ <- lift . R.string $ "MULT"
                 lift R.skipSpaces
                 val1 <- readPrec
                 lift R.skipSpaces
                 Mul val1 <$> readPrec
             )

getVal :: [Op] -> Int -> Maybe Int
getVal ops n = case op of
  Value (Val i) -> Just i
  Value (Ref i) -> getVal ops i
  _ -> Nothing
  where
    op = ops !! n

applyOp :: [Op] -> Int -> [Op]
applyOp ops n = case getVal ops n of
  Just _ -> ops
  _ -> case op of
    Value (Val _) -> ops
    Value (Ref i) -> applyOp (applyOp ops i) n
    Add (Val i) (Val j) -> replaceOp (Value . Val $ i + j)
    Add v (Ref i) -> case getVal ops i of
      Just k -> applyOp (replaceOp (Add v (Val k))) n
      _ -> applyOp (applyOp ops i) n
    Add (Ref i) v -> case getVal ops i of
      Just k -> applyOp (replaceOp (Add (Val k) v)) n
      _ -> applyOp (applyOp ops i) n
    Sub (Val i) (Val j) -> replaceOp (Value . Val $ i - j)
    Sub v (Ref i) -> case getVal ops i of
      Just k -> applyOp (replaceOp (Sub v (Val k))) n
      _ -> applyOp (applyOp ops i) n
    Sub (Ref i) v -> case getVal ops i of
      Just k -> applyOp (replaceOp (Sub (Val k) v)) n
      _ -> applyOp (applyOp ops i) n
    Mul (Val i) (Val j) -> replaceOp (Value . Val $ i * j)
    Mul v (Ref i) -> case getVal ops i of
      Just k -> applyOp (replaceOp (Mul v (Val k))) n
      _ -> applyOp (applyOp ops i) n
    Mul (Ref i) v -> case getVal ops i of
      Just k -> applyOp (replaceOp (Mul (Val k) v)) n
      _ -> applyOp (applyOp ops i) n
  where
    op = ops !! n
    replaceOp op' = take n ops <> (op' : drop (n + 1) ops)

evaluate :: [Op] -> [Int]
evaluate ops = map (fromMaybe 0 . getVal (foldl applyOp ops [0 .. length ops - 1])) [0 .. length ops - 1]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  n <- read <$> getLine
  replicateM n getLine >>= mapM_ print . evaluate . map read
