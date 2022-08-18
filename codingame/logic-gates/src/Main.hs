module Main (main) where

import Control.Monad
import System.IO
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.ParserCombinators.ReadPrec as RPr
import Text.Read

newtype Signal = Signal {unSignal :: [Bool]}

instance Show Signal where
  show (Signal bs) = map (\b -> if b then '-' else '_') bs

instance Read Signal where
  readPrec = do
    Signal . map (== '-') <$> (lift . RP.munch1 $ \c -> c == '_' || c == '-')

data NamedSignal = NamedSignal {nsName :: String, nsData :: Signal}

instance Show NamedSignal where
  show (NamedSignal name bs) = name <> " " <> show bs

instance Read NamedSignal where
  readPrec = do
    name <- lift . RP.munch1 $ \c -> c /= ' '
    lift RP.skipSpaces
    NamedSignal name <$> readPrec

data Op = AND | OR | XOR | NAND | NOR | NXOR deriving (Show)

instance Read Op where
  readPrec =
    ((lift . RP.string) "AND" >> return AND)
      RPr.+++ ((lift . RP.string) "OR" >> return OR)
      RPr.+++ ((lift . RP.string) "XOR" >> return XOR)
      RPr.+++ ((lift . RP.string) "NAND" >> return NAND)
      RPr.+++ ((lift . RP.string) "NOR" >> return NOR)
      RPr.+++ ((lift . RP.string) "NXOR" >> return NXOR)

data Gate = Gate {gName :: String, gOp :: Op, gA :: String, gB :: String} deriving (Show)

instance Read Gate where
  readPrec = do
    name <- lift . RP.munch1 $ \c -> c /= ' '
    lift RP.skipSpaces
    op <- readPrec
    lift RP.skipSpaces
    a <- lift . RP.munch1 $ (/= ' ')
    lift RP.skipSpaces
    Gate name op a <$> (lift . RP.munch1) (/= ' ')

applyOp :: Op -> Bool -> Bool -> Bool
applyOp AND x y = x && y
applyOp OR x y = x || y
applyOp XOR x y = x /= y
applyOp NAND x y = not $ x && y
applyOp NOR x y = not $ x || y
applyOp NXOR x y = x == y

applyGate :: [NamedSignal] -> Gate -> NamedSignal
applyGate nss g = NamedSignal (gName g) . Signal . zipWith (applyOp (gOp g)) (getAB gA) . getAB $ gB
  where
    sigs = map (\ns -> (nsName ns, nsData ns)) nss
    getAB f = maybe [] unSignal . lookup (f g) $ sigs

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  inputSignalsCount <- read <$> getLine
  gatesCount <- read <$> getLine
  signals <- map read <$> replicateM inputSignalsCount getLine
  replicateM_ gatesCount (getLine >>= print . applyGate signals . read)
