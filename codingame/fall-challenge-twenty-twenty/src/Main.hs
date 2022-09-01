module Main (main) where

import Control.Monad
import Control.Monad.State.Lazy hiding (lift)
import Data.List (minimumBy, sortBy)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Debug.Trace (trace)
import System.IO
import qualified Text.ParserCombinators.ReadP as RP
import Text.Read as R hiding (get)

data Witch = Witch {wIs :: [Int], wQuest :: Maybe Int} deriving (Show)

mkWitch :: Witch
mkWitch = Witch [] Nothing

data Command = CmdBrew Int | CmdCast Int (Maybe Int) | CmdLearn Int | CmdRest | CmdWait

instance Show Command where
  show (CmdBrew aid) = "BREW " <> show aid
  show (CmdCast aid Nothing) = "CAST " <> show aid
  show (CmdCast aid (Just n)) = "CAST " <> show aid <> " " <> show n
  show (CmdLearn aid) = "LEARN " <> show aid
  show CmdRest = "REST"
  show CmdWait = "WAIT"

data InputWitch = InputWitch {iwIs :: [Int], iwRuppes :: Int} deriving (Show)

instance Read InputWitch where
  readPrec = do
    d0 <- readPrec
    lift RP.skipSpaces
    d1 <- readPrec
    lift RP.skipSpaces
    d2 <- readPrec
    lift RP.skipSpaces
    d3 <- readPrec
    lift RP.skipSpaces
    InputWitch [d0, d1, d2, d3] <$> readPrec

data ActionType = Brew | Cast | OpponentCast | Learn deriving (Eq, Show)

instance Read ActionType where
  readPrec =
    lift $
      (RP.string "BREW" >> return Brew)
        RP.+++ (RP.string "CAST" >> return Cast)
        RP.+++ (RP.string "OPPONENT_CAST" >> return OpponentCast)
        RP.+++ (RP.string "LEARN" >> return Learn)

data Action = Action
  { aId :: Int,
    aType :: ActionType,
    aDelta :: [Int],
    aPrice :: Int,
    aTomeIndex :: Int,
    aTaxCount :: Int,
    aCastable :: Bool,
    aRepeatable :: Bool
  }
  deriving (Show)

instance Read Action where
  readPrec = do
    aid <- readPrec
    lift RP.skipSpaces
    atype <- readPrec
    lift RP.skipSpaces
    d0 <- readPrec
    lift RP.skipSpaces
    d1 <- readPrec
    lift RP.skipSpaces
    d2 <- readPrec
    lift RP.skipSpaces
    d3 <- readPrec
    lift RP.skipSpaces
    price <- readPrec
    lift RP.skipSpaces
    tomeIndex <- readPrec
    lift RP.skipSpaces
    taxCount <- readPrec
    lift RP.skipSpaces
    castable <- (== (1 :: Int)) <$> readPrec
    lift RP.skipSpaces
    Action aid atype [d0, d1, d2, d3] price tomeIndex taxCount castable . (== (1 :: Int)) <$> readPrec

data Input = Input {iActions :: [Action], iMe :: InputWitch, iOpp :: InputWitch} deriving (Show)

data Hut = Hut {hutMe :: Witch, hutActions :: [Action], hutExusted :: [Int]} deriving (Show)

mkHut :: Hut
mkHut = Hut mkWitch [] []

updateHut :: Input -> Hut -> Hut
updateHut inp hut = hut {hutMe = me, hutActions = iActions inp}
  where
    me = (hutMe hut) {wIs = iwIs . iMe $ inp}

updateQuest :: Input -> Hut -> Hut
updateQuest inp hut = case (wQuest . hutMe) hut of
  Just aid
    | not . any (\a -> aId a == aid) . iActions $ inp -> hut {hutMe = (hutMe hut) {wQuest = Nothing}}
  _ -> hut

haveIngredients :: Witch -> Action -> Bool
haveIngredients w a = all (\i -> wIs w !! i >= - (aDelta a !! i)) [0 .. 3]

castPrice :: Action -> (Int, [Int])
castPrice a = (sum . tail . aDelta $ a, map (\n -> if n > 0 then n else 0) . aDelta $ a)

resourcesToCast :: Witch -> [Action] -> Action -> Maybe (Int, [Int])
resourcesToCast me casts brew
  | null absentIngs = Just (0, [])
  | null castsToObtain = Nothing
  | otherwise = case resourcesToCast (me {wIs = zipWith (+) (wIs me) (aDelta . head $ castsToObtain)}) casts brew of
    Just (0, []) -> Just (1, [aId . head $ castsToObtain])
    Just (cost, aids) -> Just (cost + 1, (aId . head) castsToObtain : aids)
    _ -> Nothing
  where
    absentIngs =
      map (\(i, have, need) -> (i, need - have))
        . filter (\(_, have, need) -> have < (- need))
        $ zip3 [0 ..] (wIs me) (aDelta brew)
    castsToObtain =
      sortBy (\c1 c2 -> (sum . aDelta) c2 `compare` (sum . aDelta) c1)
        . filter (\cast -> aDelta cast !! (fst . head) absentIngs > 0)
        $ casts

rest :: State Hut Command
rest = do
  modify (\hut' -> hut' {hutExusted = []})
  return CmdRest

learnFor :: Action -> [Action] -> Action
learnFor brew = minimumBy (\t1 t2 -> diff t1 `compare` diff t2)
  where
    bdelta = map (\x -> if x > 0 then 0 else x) . aDelta $ brew
    diff = sum . zipWith (+) bdelta . aDelta

getPrice :: Action -> Int
getPrice a = aPrice a + if aTomeIndex a > 0 then aTomeIndex a else 0

gameRound :: Input -> State Hut Command
gameRound input = do
  modify (updateHut input)
  modify (updateQuest input)
  hut <- get
  let actions =
        sortBy
          ( \a1 a2 -> case getPrice a2 `compare` getPrice a1 of
              EQ -> (reverse . aDelta $ a1) `compare` (reverse . aDelta $ a2)
              r -> r
          )
          . filter (\a -> aType a == Brew && haveIngredients (hutMe hut) a)
          . hutActions
          $ hut
      aids = map aId actions
      brews =
        sortBy
          (\a1 a2 -> (aPrice . fst) a2 `compare` (aPrice . fst) a1)
          . filter (isJust . snd)
          . map (\a -> (a, resourcesToCast (hutMe hut) (filter ((== Cast) . aType) . hutActions $ hut) a))
          . filter (\a -> aType a == Brew)
          . hutActions
          $ hut
      brewCastId =
        head
          . filter
            ( \aid ->
                all (>= 0)
                  . zipWith (+) (wIs . hutMe $ hut)
                  . aDelta
                  . head
                  . filter (\a' -> aId a' == aid)
                  . hutActions
                  $ hut
            )
          . maybe [] snd
          . snd
          . head
          $ brews
      questId = aId . fst . head $ brews
      casts =
        sortBy (\a1 a2 -> castPrice a2 `compare` castPrice a1)
          . filter (\a -> aType a == Cast && aId a `notElem` hutExusted hut && haveIngredients (hutMe hut) a)
          . hutActions
          $ hut
      castCastId = aId . head $ casts
      tomes = filter (\a -> aType a == Learn) . hutActions $ hut
      goodToLearn = learnFor (fst . head $ brews) tomes
  trace (show hut) $
    if (not . null) actions && ((isNothing . wQuest . hutMe) hut || (fromMaybe 0 . wQuest . hutMe) hut `elem` aids)
      then (return . CmdBrew . aId . head) actions
      else do
        if (not . null) brews
          then
            if (length . snd . fromMaybe (0, []) . snd . head) brews > 5 && ((!! 0) . wIs . hutMe) hut >= aTomeIndex goodToLearn
              then return . CmdLearn . aId $ goodToLearn
              else
                if brewCastId `elem` hutExusted hut
                  then rest
                  else do
                    modify (\hut' -> hut' {hutExusted = brewCastId : hutExusted hut', hutMe = (hutMe hut') {wQuest = Just questId}})
                    return $ CmdCast brewCastId Nothing
          else
            if (not . null) casts && castCastId `notElem` hutExusted hut
              then do
                modify (\hut' -> hut' {hutExusted = castCastId : hutExusted hut'})
                return $ CmdCast castCastId Nothing
              else
                if (not . null . hutExusted) hut
                  then rest
                  else return CmdWait

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  -- game loop
  foldM_
    ( \hut cycle -> do
        actioncount <- read <$> getLine -- the number of spells and recipes in play
        actions <- replicateM actioncount $ read <$> getLine
        me <- read <$> getLine
        opp <- read <$> getLine
        let input = Input actions me opp
            (command, hut') = runState (gameRound input) hut
        print command
        return hut'
    )
    mkHut
    [0 ..]
