{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.List as L
import Data.Text (Text, intercalate, pack, splitOn, unpack)
import System.IO

data Value = Val Rational | Error | NotEvaluated deriving (Eq)

isVal :: Value -> Bool
isVal (Val _) = True
isVal _ = False

isError :: Value -> Bool
isError Error = True
isError _ = False

instance Show Value where
  show Error = "Error"
  show NotEvaluated = "Not evaluated"
  show (Val r) = unpack . intercalate "/" . splitOn " % " . pack . show $ r

data Vertex = Vertex {vName :: Text, vValue :: Value, vHide :: Bool, vValidator :: Rational -> Bool}

instance Show Vertex where
  show v = unpack $ vName v <> " = " <> (pack . show . vValue) v

data CType = Mul | Add deriving (Show)

data Connector = Connector {cType :: CType, cFirst :: Text, cSecond :: Text, cRes :: Text} deriving (Show)

data Propagator = Propagator {pVertices :: [Vertex], pConnectors :: [Connector]}

instance Show Propagator where
  show p
    | (any (isError . vValue) . pVertices) p = "IMPOSSIBLE"
    | otherwise = L.intercalate "\n" . map show . filter (\v -> (not . vHide) v && (isVal . vValue) v) . pVertices $ p

-- show = L.intercalate "\n" . map show . pVertices

lookupVertex :: Text -> Propagator -> Maybe Vertex
lookupVertex name p
  | null vs = Nothing
  | otherwise = Just . head $ vs
  where
    vs = filter (\v -> vName v == name) . pVertices $ p

isProb :: Rational -> Bool
isProb r = r >= 0 && r <= 1

eval :: Propagator -> Connector -> Propagator
eval p c = case (lookupVertex (cFirst c) p, lookupVertex (cSecond c) p, lookupVertex (cRes c) p) of
  (Nothing, _, _) -> p
  (_, Nothing, _) -> p
  (_, _, Nothing) -> p
  (Just v1, Just v2, Just vr) -> case (vValue v1, vValue v2, vValue vr) of
    (Error, _, _) -> p
    (_, Error, _) -> p
    (_, _, Error) -> p
    (NotEvaluated, NotEvaluated, _) -> p
    (_, NotEvaluated, NotEvaluated) -> p
    (NotEvaluated, _, NotEvaluated) -> p
    (Val x, Val y, Val z) -> case cType c of
      Mul
        | x * y == z -> p
        | otherwise -> setError (vName v1) . setError (vName v2) . setError (vName vr) $ p
      Add
        | x + y == z -> p
        | otherwise -> setError (vName v1) . setError (vName v2) . setError (vName vr) $ p
    (Val x, Val y, NotEvaluated) -> case cType c of
      Mul -> setVertex (vName vr) (x * y) p
      Add -> setVertex (vName vr) (x + y) p
    (Val x, NotEvaluated, Val z) -> case cType c of
      Mul
        | x /= 0 -> setVertex (vName v2) (z / x) p
        | otherwise -> setError (vName v2) p
      Add -> setVertex (vName v2) (z - x) p
    (NotEvaluated, Val y, Val z) -> case cType c of
      Mul
        | y /= 0 -> setVertex (vName v1) (z / y) p
        | otherwise -> setError (vName v1) p
      Add -> setVertex (vName v1) (z - y) p

setError :: Text -> Propagator -> Propagator
setError name p = p {pVertices = map (\v -> if vName v == name then v {vValue = Error} else v) . pVertices $ p}

setVertex :: Text -> Rational -> Propagator -> Propagator
setVertex name val p = case lookupVertex name p of
  Nothing -> p
  Just v
    | vValidator v val -> foldl eval p' connectors
    | otherwise -> setError name p
  where
    p' = p {pVertices = map (\v -> if vName v == name then v {vValue = Val val} else v) . pVertices $ p}
    connectors = filter (\c -> cFirst c == name || cSecond c == name || cRes c == name) . pConnectors $ p

parseInputLine :: String -> (Text, Rational)
parseInputLine =
  (\ts -> (head ts, read . unpack . intercalate "%" . splitOn "/" . last $ ts))
    . splitOn " = "
    . pack

myPropagator :: Propagator
myPropagator =
  Propagator
    { pVertices =
        [ Vertex {vName = "1", vValue = Val 1, vHide = True, vValidator = const True},
          Vertex {vName = "0", vValue = Val 0, vHide = True, vValidator = const True},
          Vertex {vName = "A", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "A AND B", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "A AND NOT B", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "A GIVEN B", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "A GIVEN NOT B", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "B", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "B GIVEN A", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "B GIVEN NOT A", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "NOT A", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "NOT A AND B", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "NOT A AND NOT B", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "NOT A GIVEN B", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "NOT A GIVEN NOT B", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "NOT B", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "NOT B GIVEN A", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "NOT B GIVEN NOT A", vValue = NotEvaluated, vHide = False, vValidator = isProb},
          Vertex {vName = "1 - A", vValue = NotEvaluated, vHide = True, vValidator = const True},
          Vertex {vName = "1 - A - B", vValue = NotEvaluated, vHide = True, vValidator = const True}
        ],
      pConnectors =
        [ Connector {cType = Add, cFirst = "1 - A", cSecond = "A", cRes = "1"},
          Connector {cType = Add, cFirst = "1 - A - B", cSecond = "B", cRes = "1 - A"},
          Connector {cType = Add, cFirst = "1 - A - B", cSecond = "A AND B", cRes = "NOT A AND NOT B"},
          Connector {cType = Add, cFirst = "A AND NOT B", cSecond = "A AND B", cRes = "A"},
          Connector {cType = Add, cFirst = "NOT A AND B", cSecond = "A AND B", cRes = "B"},
          Connector {cType = Add, cFirst = "NOT A", cSecond = "A", cRes = "1"},
          Connector {cType = Add, cFirst = "NOT B", cSecond = "B", cRes = "1"},
          Connector {cType = Mul, cFirst = "A GIVEN B", cSecond = "B", cRes = "A AND B"},
          Connector {cType = Mul, cFirst = "B GIVEN A", cSecond = "A", cRes = "A AND B"},
          Connector {cType = Mul, cFirst = "A GIVEN NOT B", cSecond = "NOT B", cRes = "A AND NOT B"},
          Connector {cType = Mul, cFirst = "B GIVEN NOT A", cSecond = "NOT A", cRes = "NOT A AND B"},
          Connector {cType = Mul, cFirst = "NOT A GIVEN B", cSecond = "B", cRes = "NOT A AND B"},
          Connector {cType = Mul, cFirst = "NOT A GIVEN NOT B", cSecond = "NOT B", cRes = "NOT A AND NOT B"},
          Connector {cType = Mul, cFirst = "NOT B GIVEN A", cSecond = "A", cRes = "A AND NOT B"},
          Connector {cType = Mul, cFirst = "NOT B GIVEN NOT A", cSecond = "NOT A", cRes = "NOT A AND NOT B"}
        ]
    }

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  (name1, val1) <- parseInputLine <$> getLine
  (name2, val2) <- parseInputLine <$> getLine
  (name3, val3) <- parseInputLine <$> getLine

  print . setVertex name1 val1 . setVertex name2 val2 . setVertex name3 val3 $ myPropagator
