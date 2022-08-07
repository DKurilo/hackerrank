import Control.Monad
import Data.List (maximumBy, minimumBy)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import System.IO

newtype Ash = Ash {ashP :: Point} deriving (Eq, Show)

data Point = Point {pX :: Int, pY :: Int} deriving (Eq)

instance Show Point where
  show (Point x y) = show x <> " " <> show y

data Human = Human {hId :: Int, hP :: Point} deriving (Eq, Show)

data Zombie = Zombie {zId :: Int, zP :: Point, zPNext :: Point} deriving (Eq, Show)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt d2
  where
    d2 = fromIntegral ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))

centroid :: [Point] -> Point
centroid ps = Point (middle pX ps) (middle pY ps)
  where
    k = length ps
    middle f = (`div` k) . sum . map f

closestToZombies :: Ash -> [Human] -> [Zombie] -> Maybe Point
closestToZombies ash hs [] = Nothing
closestToZombies ash hs zs
  | null zsOnStep && null huntForAsh = (Just . centroid . map zP) cantSave
  | null zsOnStep = (Just . centroid . map zP) huntForAsh
  | null tooFar = Just center
  | otherwise = closestToZombies ash hs . filter (\z -> zId z /= zId furthestFromHumans) $ zs
  where
    distanceToHuman z = (fst hAndDistance, snd hAndDistance, z)
      where
        hAndDistance = minimumBy (\h1 h2 -> compare (snd h1) (snd h2)) . map (\h -> (h, distance (zP z) . hP $ h)) $ hs
    zsAndDistanceToHuman = map distanceToHuman zs
    closestToHuman = minimumBy (\(_, d1, _) (_, d2, _) -> compare d1 d2) zsAndDistanceToHuman
    huntForAsh =
      map (\(_, _, z) -> z)
        . filter (\(h, d, z) -> abs (d - (\(_, d1, _) -> d1) closestToHuman) > 100 && d >= distance (ashP ash) (zP z))
        $ zsAndDistanceToHuman
    huntForAshIds = map zId huntForAsh
    cantSave =
      map (\(_, _, z) -> z)
        . filter (\(h, d, z) -> d / 400 < ((distance (hP h) (ashP ash) - 2400) / 1000))
        $ zsAndDistanceToHuman
    cantSaveIds = map zId cantSave
    steps = round $ (\(_, d, _) -> d) closestToHuman / 400
    doSteps z = z {zPNext = Point (max 0 (x + stepX * steps)) (max 0 (y + stepY * steps))}
      where
        p = zP z
        x = pX p
        y = pY p
        pNext = zPNext z
        xNext = pX pNext
        yNext = pY pNext
        stepX = xNext - x
        stepY = yNext - y
    zsOnStep = map doSteps . filter (\z -> zId z `notElem` cantSaveIds) . filter (\z -> zId z `notElem` huntForAshIds) $ zs
    zsOnStepAndDistanceToHuman = map distanceToHuman zsOnStep
    furthestFromHumans = (\(_, _, z) -> z) . maximumBy (\(_, d1, _) (_, d2, _) -> compare d1 d2) $ zsOnStepAndDistanceToHuman
    center = centroid . map zPNext $ zsOnStep
    dist z = distance center (zPNext z)
    tooFar = filter (\z -> dist z >= 2000) zsOnStep

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  -- Save humans, destroy zombies!

  -- game loop
  forever $ do
    input_line <- getLine
    let input = words input_line
    let x = read (head input) :: Int
    let y = read (input !! 1) :: Int
    let ash = Ash (Point x y)
    input_line <- getLine
    let humancount = read input_line :: Int

    humans <- replicateM
      humancount
      $ do
        input_line <- getLine
        let input = words input_line
        let humanid = read (head input) :: Int
        let humanx = read (input !! 1) :: Int
        let humany = read (input !! 2) :: Int
        return (Human humanid (Point humanx humany))

    input_line <- getLine
    let zombiecount = read input_line :: Int

    zombies <- replicateM zombiecount $ do
      input_line <- getLine
      let input = words input_line
      let zombieid = read (head input) :: Int
      let zombiex = read (input !! 1) :: Int
      let zombiey = read (input !! 2) :: Int
      let zombiexnext = read (input !! 3) :: Int
      let zombieynext = read (input !! 4) :: Int
      return (Zombie zombieid (Point zombiex zombiey) (Point zombiexnext zombieynext))

    -- hPutStrLn stderr "Debug messages..."

    -- Your destination coordinates
    print . fromMaybe (Point 0 0) . closestToZombies ash humans $ zombies
