import Control.Monad
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)
import System.IO
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.ParserCombinators.ReadPrec as R
import Text.Read

data Resistor = Resistor {rName :: String, rValue :: Rational} deriving (Show)

instance Read Resistor where
  readPrec = do
    name <- lift . RP.munch1 $ isAlpha
    lift RP.skipSpaces
    Resistor name . (toRational :: Double -> Rational) <$> readPrec

data Circuit = Par [Circuit] | Seq [Circuit] | Res String deriving (Show)

instance Read Circuit where
  readPrec =
    ( Par
        <$> lift
          ( do
              RP.skipSpaces
              _ <- RP.char '['
              cs <-
                RP.many1 $
                  do
                    RP.skipSpaces
                    circ <- readPrec_to_P readPrec 0
                    RP.skipSpaces
                    return circ
              _ <- RP.char ']'
              RP.skipSpaces
              return cs
          )
    )
      R.+++ ( Seq
                <$> lift
                  ( do
                      RP.skipSpaces
                      _ <- RP.char '('
                      cs <-
                        RP.many1 $
                          do
                            RP.skipSpaces
                            circ <- readPrec_to_P readPrec 0
                            RP.skipSpaces
                            return circ
                      _ <- RP.char ')'
                      RP.skipSpaces
                      return cs
                  )
            )
      R.+++ ( Res
                <$> ( do
                        lift RP.skipSpaces
                        name <- lift . RP.munch1 $ isAlpha
                        lift RP.skipSpaces
                        return name
                    )
            )

evaluate :: Circuit -> [Resistor] -> Rational
evaluate c rs = case c of
  Par cs -> 1 / (sum . map (\c' -> 1 / evaluate c' rs)) cs
  Seq cs -> sum . map (`evaluate` rs) $ cs
  Res name -> fromMaybe 0 . lookup name $ rs'
  where
    rs' = map (\r -> (rName r, rValue r)) rs

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  resistorsCount <- read <$> getLine

  resistors <- replicateM resistorsCount $ read <$> getLine
  circuit <- read <$> getLine
  let value = show . (+ (0.05 :: Double)) . fromRational $ evaluate circuit resistors
  putStrLn $ takeWhile (/= '.') value <> (take 2 . dropWhile (/= '.')) value
