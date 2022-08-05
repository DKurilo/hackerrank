import Control.Monad
import Data.Bifunctor (Bifunctor (second))
import Data.List (maximumBy, nub)
import Data.Map (Map (..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set (Set (..))
import qualified Data.Set as S
import Debug.Trace (trace)
import System.IO

data Direction = DNoMove | DUp | DDown | DRight | DLeft deriving (Eq, Ord, Show)

data Coord = Coord {coordX :: Int, coordY :: Int} deriving (Show, Eq, Ord)

data Snapshot = Snapshot
  { sWidth :: Int,
    sHeight :: Int,
    sRounds :: Int,
    sBombs :: Int,
    sPass :: Set Coord,
    sSrv :: [(Coord, [Direction])]
  }
  deriving (Eq, Show)

data Srv = Srv {srvId :: Int, srvCoord :: Coord, srvDir :: Direction} deriving (Show, Eq, Ord)

data Grid = Grid {gWidth :: Int, gHeight :: Int, gRounds :: Int, gBombs :: Int, gSrv :: Set Srv, gPass :: Set Coord} deriving (Show)

data Action = Wait | Bomb Coord

instance Show Action where
  show Wait = "WAIT"
  show (Bomb (Coord x y)) = show x <> " " <> show y

isCollision :: Coord -> Int -> Int -> Set Coord -> Bool
isCollision c@(Coord x y) w h ps = x < 0 || y < 0 || x >= w || y >= h || c `S.member` ps

opposite :: Direction -> Direction
opposite DNoMove = DNoMove
opposite DUp = DDown
opposite DDown = DUp
opposite DLeft = DRight
opposite DRight = DLeft

applyDirection :: Direction -> Coord -> Int -> Int -> Set Coord -> (Coord, Direction)
applyDirection d c w h ps
  | not (isCollision c' w h ps) = (c', d)
  | not (isCollision cop w h ps) = (cop, dop)
  | otherwise = (c, DNoMove)
  where
    nextCoord :: Direction -> Coord -> Coord
    nextCoord d' (Coord x' y') = case d' of
      DNoMove -> Coord x' y'
      DUp -> Coord x' (y' - 1)
      DDown -> Coord x' (y' + 1)
      DLeft -> Coord (x' - 1) y'
      DRight -> Coord (x' + 1) y'
    c' = nextCoord d c
    dop = opposite d
    cop = nextCoord dop c

detectMovements :: Snapshot -> IO Grid
detectMovements (Snapshot w h r b passive srvs) =
  if all (\(_, ds) -> length ds == 1) srvs
    then return (Grid w h r b (S.fromList . zipWith (\n (crd, [d]) -> Srv n crd d) [0 ..] $ srvs) passive)
    else do
      print Wait
      let knownLocations =
            map
              ( \(crd, [d]) -> second (: []) $ applyDirection d crd w h passive
              )
              . filter (\(_, ds) -> length ds == 1)
              $ srvs
          knownCoords = map fst knownLocations
          possibleLocations =
            concatMap (\(c, ds) -> map (\d -> applyDirection d c w h passive) ds)
              . filter ((> 1) . length . snd)
              $ srvs
      [rounds, bombs] <- map read . words <$> getLine
      srvCs <-
        map (\(_, x, y) -> Coord x y)
          . filter (\(c, _, _) -> c == '@')
          . concat
          <$> forM [0 .. h - 1] (\y -> zipWith (\x c -> (c, x, y)) [0 ..] <$> getLine)
      let movedSrvs = filter (not . (`elem` knownCoords)) srvCs
          predictions = map (\c -> (c, nub . map snd . filter (\(c', _) -> c == c') $ possibleLocations)) movedSrvs
          srvs' = knownLocations ++ predictions
      detectMovements (Snapshot w h rounds bombs passive srvs')

defineGrid :: Int -> Int -> IO Grid
defineGrid w h = do
  _ <- getLine
  snapshot <-
    filter (\(c, _, _) -> c == '#' || c == '@') . concat
      <$> forM [0 .. h -1] (\y -> zipWith (\x c -> (c, x, y)) [0 ..] <$> getLine)
  let coords (_, x, y) = Coord x y
      filterC c = map coords . filter (\(c', _, _) -> c' == c)
      mkSrv crd@(Coord x y) =
        ( crd,
          [ d
            | (d, x', y') <-
                [ (DNoMove, x, y),
                  (DUp, x, y - 1),
                  (DDown, x, y + 1),
                  (DLeft, x - 1, y),
                  (DRight, x + 1, y)
                ],
              x' >= 0,
              x' < w,
              y' >= 0,
              y' < h
          ]
        )
  detectMovements (Snapshot w h 0 0 (S.fromList . filterC '#' $ snapshot) (map mkSrv . filterC '@' $ snapshot))

nextRound :: Grid -> Grid
nextRound (Grid w h r b srvs ps) = Grid w h (r - 1) b srvs' ps
  where
    srvs' = S.map (\(Srv sid c d) -> let (c', d') = applyDirection d c w h ps in Srv sid c' d') srvs

canPlaceBomb :: Set Coord -> Coord -> Bool
canPlaceBomb badCoords c = S.notMember c badCoords

explosion :: Set Coord -> Coord -> Set Coord
explosion ps c =
  selfDestruction `S.union` rightDestruction `S.union` leftDestruction `S.union` upDestruction `S.union` downDestruction
  where
    directedDestruction :: (Int -> Coord) -> Set Coord
    directedDestruction f =
      S.fromList
        . takeWhile (`S.notMember` ps)
        . map f
        $ [1, 2, 3]
    rightDestruction = directedDestruction (\n -> c {coordX = coordX c + n})
    leftDestruction = directedDestruction (\n -> c {coordX = coordX c - n})
    upDestruction = directedDestruction (\n -> c {coordY = coordY c - n})
    downDestruction = directedDestruction (\n -> c {coordY = coordY c + n})
    selfDestruction = S.fromList [c]

destroyedCount :: Map Coord [Int] -> Set Coord -> Coord -> Set Int
destroyedCount mSrvs ps = S.fromList . concat . M.elems . M.restrictKeys mSrvs . explosion ps

findBestBombCoord :: Grid -> Grid -> Set Coord -> Set Coord -> (Coord, Set Int)
findBestBombCoord gBefore g forbiddenPlaces explosions =
  maximumBy (\(_, a) (_, b) -> compare (S.size a) (S.size b))
    . map (\c -> (c, destroyedCount mSrvs ps c))
    $ [ Coord x y
        | x <- [0 .. gWidth g - 1],
          y <- [0 .. gHeight g - 1],
          let c = Coord x y,
          c `S.notMember` forbiddenPlaces && c `S.notMember` explosions && canPlaceBomb badCoords c
      ]
  where
    ps = gPass g
    mSrvs = M.fromListWith (++) . map (\srv -> (srvCoord srv, [srvId srv])) . S.toList . gSrv $ g
    badCoords = S.union (gPass gBefore) (S.map srvCoord . gSrv $ gBefore)

planActions :: Grid -> Set Int -> Set Coord -> Map Int (Set Coord) -> Maybe [(Int, Action)]
planActions g removedRounds forbiddenPlaces explosions
  | (S.null . gSrv) g = Just []
  | gBombs g == 0 = Nothing
  | null possiblePlaces = Nothing
  | S.null destroyed = Nothing
  | isJust mbRestOfActions = Just ((placeRound, Bomb bestPlaceForBomb) : restOfActions)
  | S.size destroyed < 4 = Nothing
  | otherwise = planActions g removedRounds (S.insert bestPlaceForBomb forbiddenPlaces) explosions
  where
    gs = take (min (gRounds g) (3000 `div` gWidth g `div` gHeight g)) . iterate nextRound $ g
    gridBeforeAndNow = filter (\(n, _, _) -> n `S.notMember` removedRounds) $ zip3 [0 ..] gs (drop 3 gs)
    possiblePlaces =
      reverse
        . filter (not . null . snd . snd)
        . map
          ( \(round, gridBefore, gridNow) ->
              let dangerousPlaces = S.unions [fromMaybe S.empty . M.lookup (round + n) $ explosions | n <- [0 .. 2]]
               in (round, findBestBombCoord gridBefore gridNow forbiddenPlaces dangerousPlaces)
          )
        $ gridBeforeAndNow
    (placeRound, (bestPlaceForBomb, destroyed)) =
      maximumBy (\(_, (_, a)) (_, (_, b)) -> compare (S.size a) (S.size b)) possiblePlaces
    g' = g {gBombs = gBombs g - 1, gSrv = S.filter ((`S.notMember` destroyed) . srvId) . gSrv $ g}
    mbRestOfActions =
      planActions
        g'
        (S.insert placeRound removedRounds)
        forbiddenPlaces
        (M.insert (placeRound + 2) (explosion (gPass g) bestPlaceForBomb) explosions)
    Just restOfActions = mbRestOfActions

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE
  input <- words <$> getLine
  let width = read (head input) :: Int
  let height = read (input !! 1) :: Int
  grid <- defineGrid width height
  let actions = fromMaybe [] $ planActions grid S.empty S.empty M.empty
  forM_ [0 ..] $ \rnd -> do
    print . head $ ((map snd . filter ((== rnd) . fst) $ actions) ++ [Wait])
    input_line <- getLine
    let input = words input_line
    let rounds = read (head input) :: Int
    let bombs = read (input !! 1) :: Int
    replicateM_ height $ do
      _ <- getLine
      return ()
