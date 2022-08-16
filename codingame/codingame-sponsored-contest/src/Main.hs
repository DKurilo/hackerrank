{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.List (intercalate, sortBy)
import Data.Map.Strict (Map (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set (..))
import qualified Data.Set as S
import Debug.Trace (trace)
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

data Thing = Wall | Empty | Teleport Coord deriving (Eq, Show)

data ThingsAround = ThingsAround {taUp :: Thing, taLeft :: Thing, taDown :: Thing, taRight :: Thing} deriving (Show)

instance Read Thing where
  readPrec = ((lift . TP.char) '#' >> return Wall) +++ ((lift . TP.char) '_' >> return Empty)

data Direction = DUp | DLeft | DDown | DRight | DWait deriving (Eq)

instance Show Direction where
  show DUp = "C"
  show DLeft = "E"
  show DDown = "D"
  show DRight = "A"
  show DWait = "B"

data Labirynth = Labirynth
  { llab :: Map Coord (Thing, Int),
    lw :: Int,
    lh :: Int,
    lc :: Int,
    lcreat :: [Coord],
    lme :: Coord
  }

instance Show Labirynth where
  show (Labirynth l w h cCount creats me) =
    intercalate
      "\n"
      [ [ case (c `elem` creats, c == me, M.lookup c l) of
            (True, _, _) -> '&'
            (_, True, _) -> 'G'
            (_, _, Just (Empty, 0)) -> '*'
            (_, _, Just (Empty, n))
              | n < 10 -> head . show $ n
              | otherwise -> '+'
            (_, _, Just (Wall, _)) -> '#'
            (_, _, Just (Teleport _, n)) -> '@'
            _ -> '.'
          | x <- [-1 .. w],
            let c = Coord x y
        ]
        | y <- [-1 .. h]
      ]

canGo :: ThingsAround -> [Direction]
canGo ts = [DUp | taUp ts /= Wall] <> [DLeft | taLeft ts /= Wall] <> [DDown | taDown ts /= Wall] <> [DRight | taRight ts /= Wall]

applyDirection :: Labirynth -> Direction -> Coord -> Coord
applyDirection lab d c = case c `M.lookup` llab lab of
  Just (Teleport c'', _)
    | cY c' < 0 || cX c' < 0 || cY c' >= lh lab || cX c' >= lw lab -> c''
    | otherwise -> c'
  _ -> c'
  where
    c'
      | d == DUp = c {cY = cY c - 1}
      | d == DLeft = c {cX = cX c - 1}
      | d == DDown = c {cY = cY c + 1}
      | d == DRight = c {cX = cX c + 1}
      | otherwise = c

distance :: Labirynth -> Coord -> Coord -> Maybe Int
distance l c1 c2 = doer 0 (S.singleton c1) (S.singleton c1)
  where
    doer :: Int -> Set Coord -> Set Coord -> Maybe Int
    doer steps front visited
      | c2 `S.member` front = Just steps
      | S.null front = Just $ abs (cX c1 - cX c2) + abs (cY c1 - cY c2)
      | otherwise = doer (steps + 1) front' visited'
      where
        front' =
          S.fromList
            . concatMap
              ( \c ->
                  [ c'
                    | d <- [DUp, DDown, DLeft, DRight],
                      let c' = applyDirection l d c,
                      c' `S.notMember` visited,
                      case c' `M.lookup` llab l of
                        Just (Wall, _) -> False
                        Nothing -> False
                        _ -> True
                  ]
              )
            . S.toList
            $ front
        visited' = S.union visited front'

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  height <- read <$> getLine :: IO Int
  width <- read <$> getLine :: IO Int
  creaturesAmount <- read <$> getLine

  -- game loop
  foldM_
    ( \(lab, cOld, cCalc) round -> do
        whatUp <- read <$> getLine
        whatRight <- read <$> getLine
        whatDown <- read <$> getLine
        whatLeft <- read <$> getLine

        (me : creatures) <- reverse <$> replicateM creaturesAmount (read <$> getLine)

        let cMeUp = applyDirection lab DUp me
            cMeDown = applyDirection lab DDown me
            cMeLeft = applyDirection lab DLeft me
            cMeRight = applyDirection lab DRight me
            whatUp' = case M.lookup cMeUp (llab lab) of
              Just (t, _) -> t
              _ -> whatUp
            whatDown' = case M.lookup cMeDown (llab lab) of
              Just (t, _) -> t
              _ -> whatDown
            whatLeft' = case M.lookup cMeLeft (llab lab) of
              Just (t, _) -> t
              _ -> whatLeft
            whatRight' = case M.lookup cMeRight (llab lab) of
              Just (t, _) -> t
              _ -> whatRight
            thingsAround = ThingsAround whatUp' whatLeft' whatDown' whatRight'
            lab' =
              lab
                { llab =
                    M.unionWith
                      ( \(tOld, v1) (tNew, v2) -> case (tOld, tNew) of
                          (Teleport _, _) -> (tOld, v1 + v2)
                          (_, Teleport _) -> (tNew, v1 + v2)
                          _ -> (tNew, v1 + v2)
                      )
                      (llab lab)
                      ( M.fromListWith
                          ( \v1 v2 -> case (v1, v2) of
                              (_, (Teleport _, _)) -> v2
                              _ -> v1
                          )
                          ( map (,(Empty, 0)) creatures
                              <> [ (me, (Empty, 1)),
                                   (cMeUp, (whatUp, 0)),
                                   (cMeDown, (whatDown, 0)),
                                   (cMeLeft, (whatLeft, 0)),
                                   (cMeRight, (whatRight, 0))
                                 ]
                              <> join
                                [ [(me, (Teleport cOld, 1)), (cOld, (Teleport me, 0))]
                                  | isJust cCalc && cCalc /= Just me || abs (cX cOld - cX me) > 1 || abs (cY cOld - cY me) > 1,
                                    let (Just c) = cCalc
                                ]
                          )
                      ),
                  lcreat = creatures,
                  lme = me
                }

        hPrint stderr (width, height, creaturesAmount, creatures, me)
        hPrint stderr thingsAround
        let sumCreatureDistance c =
              ( \ds ->
                  let cnt = sum . map fst $ ds
                      sm = sum . map snd $ ds
                   in fromIntegral sm / fromIntegral cnt
              )
                . map (maybe (0, 0) (1,) . distance lab' c)
                $ creatures
            closestCreatureDistance c = minimum . map (fromMaybe 10000 . distance lab' c) $ creatures
            possible =
              map (\(c, _, _, _, _) -> c)
                . sortBy
                  ( \(d1, c1, scd1, cd1, mbTv1) (d2, c2, scd2, cd2, mbTv2) ->
                      let oscd = compare scd2 scd1
                          ocd = compare cd2 cd1
                       in if cd1 < 2 || cd2 < 2
                            then case ocd of
                              EQ -> case (mbTv1, mbTv2) of
                                (Just tv, Nothing)
                                  | snd tv > 0 -> GT
                                  | otherwise -> LT
                                (Nothing, Just tv)
                                  | snd tv > 0 -> LT
                                  | otherwise -> GT
                                (Nothing, Nothing) -> ocd
                                (Just tv1, Just tv2)
                                  | snd tv1 == snd tv2 -> ocd
                                  | snd tv1 > snd tv2 -> GT
                                  | otherwise -> LT
                              _ -> ocd
                            else case (mbTv1, mbTv2) of
                              (Just tv, Nothing)
                                | snd tv > 0 -> GT
                                | otherwise -> LT
                              (Nothing, Just tv)
                                | snd tv > 0 -> LT
                                | otherwise -> GT
                              (Nothing, Nothing) -> oscd
                              (Just tv1, Just tv2)
                                | snd tv1 == snd tv2 -> oscd
                                | snd tv1 > snd tv2 -> GT
                                | otherwise -> LT
                  )
                . map
                  ( \d ->
                      let c = applyDirection lab' d me
                          scd = sumCreatureDistance c
                          cd = closestCreatureDistance c
                          mbTv = c `M.lookup` llab lab'
                       in (d, c, scd, cd, mbTv)
                  )
                . canGo
                $ thingsAround
        hPrint stderr possible
        hPrint stderr lab'
        let newDir = head possible
            newMe = applyDirection lab newDir me
        print newDir
        return (lab', me, Just newMe)
    )
    (Labirynth M.empty width height creaturesAmount [] (Coord 0 0), Coord 0 0, Nothing)
    [0 ..]
