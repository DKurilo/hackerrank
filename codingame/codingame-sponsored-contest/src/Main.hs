{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.List (intercalate, sortBy)
import Data.Map (Map (..))
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import System.IO
import qualified Text.ParserCombinators.ReadP as TP
import Text.Read

data Coord = Coord {cX :: Int, cY :: Int} deriving (Eq, Ord)

instance Show Coord where
  show (Coord x y) = "(" <> show x <> ", " <> show y <> ")"

instance Read Coord where
  readPrec = do
    x <- readPrec
    lift TP.skipSpaces
    Coord x <$> readPrec

data Thing = Wall | Empty deriving (Eq)

data ThingsAround = ThingsAround {taUp :: Thing, taLeft :: Thing, taDown :: Thing, taRight :: Thing} deriving (Show)

instance Read Thing where
  readPrec = ((lift . TP.char) '#' >> return Wall) +++ ((lift . TP.char) '_' >> return Empty)

instance Show Thing where
  show Wall = "#"
  show Empty = "."

data Direction = DUp | DLeft | DDown | DRight | DUnknown deriving (Eq)

instance Show Direction where
  show DUp = "C"
  show DLeft = "E"
  show DDown = "D"
  show DRight = "A"
  show DUnknown = "B"

data Labirynth = Labirynth {llab :: Map Coord (Thing, Int), lw :: Int, lh :: Int}

instance Show Labirynth where
  show (Labirynth l w h) =
    intercalate
      "\n"
      [ [ case M.lookup (Coord x y) l of
            Just (Empty, 0) -> '*'
            Just (Empty, n)
              | n < 10 -> head . show $ n
              | otherwise -> '+'
            Just (Wall, _) -> '#'
            _ -> '.'
          | x <- [minX .. maxX]
        ]
        | y <- [minY .. maxY]
      ]
    where
      minX = S.findMin . S.map cX . M.keysSet $ l
      maxX = S.findMax . S.map cX . M.keysSet $ l
      minY = S.findMin . S.map cY . M.keysSet $ l
      maxY = S.findMax . S.map cY . M.keysSet $ l

canGo :: ThingsAround -> [Direction]
canGo ts = [DUp | taUp ts == Empty] <> [DLeft | taLeft ts == Empty] <> [DDown | taDown ts == Empty] <> [DRight | taRight ts == Empty]

applyDirection :: Direction -> Coord -> Coord
applyDirection DUp c = c {cY = cY c - 1}
applyDirection DLeft c = c {cX = cX c - 1}
applyDirection DDown c = c {cY = cY c + 1}
applyDirection DRight c = c {cX = cX c + 1}
applyDirection DUnknown c = c

distance :: Coord -> Coord -> Int
distance c1 c2 = (cX c1 - cX c2) * (cX c1 - cX c2) + (cY c1 - cY c2) * (cY c1 - cY c2)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  height <- read <$> getLine :: IO Int
  width <- read <$> getLine :: IO Int
  creaturesAmount <- read <$> getLine

  -- game loop
  foldM_
    ( \(lab, cCalc) round -> do
        whatUp <- read <$> getLine
        whatRight <- read <$> getLine
        whatDown <- read <$> getLine
        whatLeft <- read <$> getLine

        (me : creatures) <- reverse <$> replicateM creaturesAmount (read <$> getLine)

        let cMeUp = applyDirection DUp me
            cMeDown = applyDirection DDown me
            cMeLeft = applyDirection DLeft me
            cMeRight = applyDirection DRight me
            thingsAround = ThingsAround whatUp whatLeft whatDown whatRight
            lab' =
              lab
                { llab =
                    M.unionWith
                      (\(_, v1) (t, v2) -> (t, v1 + v2))
                      (llab lab)
                      ( M.fromList
                          ( map (,(Empty, 0)) creatures
                              <> [ (me, (Empty, 1)),
                                   (cMeUp, (whatUp, 0)),
                                   (cMeDown, (whatDown, 0)),
                                   (cMeLeft, (whatLeft, 0)),
                                   (cMeRight, (whatRight, 0))
                                 ]
                              <> [(c, (Empty, 1)) | isJust cCalc && cCalc /= Just me, let (Just c) = cCalc]
                          )
                      )
                }

        hPrint stderr (width, height, creaturesAmount, creatures, me)
        hPrint stderr thingsAround
        let sumCreatureDistance c = sum . map (distance c) $ creatures
            closestCreatureDistance c = minimum . map (distance c) $ creatures
            possible =
              sortBy
                ( \d1 d2 ->
                    let c1 = applyDirection d1 me
                        c2 = applyDirection d2 me
                        scd1 = sumCreatureDistance c1
                        scd2 = sumCreatureDistance c2
                        oscd = compare scd2 scd1
                        cd1 = closestCreatureDistance c1
                        cd2 = closestCreatureDistance c2
                        ocd = compare cd2 cd1
                     in if cd1 < 2 || cd2 < 2
                          then ocd
                          else case (M.lookup c1 (llab lab'), M.lookup c2 (llab lab')) of
                            (Just tv, Nothing)
                              | snd tv > 0 -> LT
                              | otherwise -> GT
                            (Nothing, Just tv)
                              | snd tv > 0 -> GT
                              | otherwise -> LT
                            (Nothing, Nothing) -> oscd
                            (Just tv1, Just tv2)
                              | snd tv1 == snd tv2 -> oscd
                              | snd tv1 > snd tv2 -> GT
                              | otherwise -> LT
                )
                . canGo
                $ thingsAround
        hPrint stderr possible
        hPrint stderr lab'
        let newDir = head possible
            newMe = applyDirection newDir me
        print newDir
        return (lab', Just newMe)
    )
    (Labirynth M.empty width height, Nothing)
    [0 ..]
