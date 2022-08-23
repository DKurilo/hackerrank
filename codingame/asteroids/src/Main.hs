import Control.Monad
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import System.IO

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

newtype Asteroid = Asteroid {unAsteroid :: Char} deriving (Eq, Ord)

instance Show Asteroid where
  show = (: []) . unAsteroid

data Sky = Sky {skyWidth :: Int, skyHeight :: Int, skyMap :: Map Asteroid Point, skyTime :: Int}

instance Show Sky where
  show sky =
    intercalate
      "\n"
      [ [ maybe '.' (head . show . fst) . M.lookupMin . M.filter (== Point x y) . skyMap $ sky
          | x <- [0 .. skyWidth sky - 1]
        ]
        | y <- [0 .. skyHeight sky -1]
      ]

data Speed = Speed
  { speedOrigin :: Point,
    speedX :: Int,
    speedY :: Int,
    speedStartTime :: Int,
    speedPer :: Int
  }
  deriving (Show)

data SpeedMap = SpeedMap {smWidth :: Int, smHeight :: Int, smMap :: Map Asteroid Speed}

mkSpeedMap :: Sky -> Sky -> SpeedMap
mkSpeedMap sky1 sky2 =
  SpeedMap
    (skyWidth sky1)
    (skyHeight sky1)
    ( M.mapWithKey
        ( \a p ->
            maybe (Speed p 0 0 t0 dt) (\p' -> Speed p (pX p' - pX p) (pY p' - pY p) t0 dt) . M.lookup a . skyMap $ sky2
        )
        . skyMap
        $ sky1
    )
  where
    t0 = skyTime sky1
    dt = skyTime sky2 - t0

skyInTime :: Int -> SpeedMap -> Sky
skyInTime time speeds =
  Sky
    (smWidth speeds)
    (smHeight speeds)
    ( M.map
        ( \sp ->
            let t0 = speedStartTime sp
                dt = time - t0
                Point x0 y0 = speedOrigin sp
                x = x0 + floor ((fromIntegral :: Int -> Double) (dt * speedX sp) / (fromIntegral . speedPer) sp)
                y = y0 + floor ((fromIntegral :: Int -> Double) (dt * speedY sp) / (fromIntegral . speedPer) sp)
             in Point x y
        )
        . smMap
        $ speeds
    )
    time

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  (w : h : t1 : t2 : t3 : _) <- map read . words <$> getLine

  (asteroids1, asteroids2) <-
    foldM
      ( \(a1, a2) i -> do
          let addAsteroids = M.fromList . filter ((/= Asteroid '.') . fst) . zipWith (\j c -> (Asteroid c, Point j i)) [0 ..]
          rows <- words <$> getLine
          let a1' = a1 `M.union` if null rows then M.empty else (addAsteroids . head) rows
              a2' = a2 `M.union` if (null . tail) rows then M.empty else (addAsteroids . (!! 1)) rows
          return (a1', a2')
      )
      (M.empty, M.empty)
      [0 .. h -1]

  print . skyInTime t3 $ mkSpeedMap (Sky w h asteroids1 t1) (Sky w h asteroids2 t2)
