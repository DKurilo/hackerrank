import Control.Monad
import Control.Monad.State.Lazy
import Debug.Trace (trace)
import System.IO

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Show)

distance :: Point -> Point -> Int
distance p1 p2 = floor . sqrt . (fromIntegral :: Int -> Double) $ (pX p1 - pX p2) * (pX p1 - pX p2) + (pY p1 - pY p2) * (pY p1 - pY p2)

data Thrust = Thrust Int | Boost

instance Show Thrust where
  show (Thrust thrust) = show thrust
  show Boost = "BOOST"

data Command = Command {cX :: Int, cY :: Int, cThrust :: Thrust}

data Bot = Bot
  { botHaveBoost :: Bool,
    botCheckPoints :: [Point],
    botIsNewCheckpoint :: Bool,
    botMinThrust :: Int,
    botCoef :: Double,
    botCriticalAngle :: Int,
    botCloseDistance :: Int,
    botCloseAngle :: Int,
    botCloseMinThurst :: Int,
    botCloseCoef :: Double,
    botLap :: Int,
    botToFinish :: Bool
  }

mkBot :: Bot
mkBot = Bot True [] False 40 400 50 5000 12 10 40 0 False

data Input = Input
  { iMe :: Point,
    iCheckpoint :: Point,
    iDistance :: Int,
    iAngle :: Int,
    iOpp :: Point,
    iCycle :: Int
  }

instance Show Command where
  show c = unwords [show . cX $ c, show . cY $ c, show . cThrust $ c]

sqEq :: Double -> Double -> Double -> (Double, Double)
sqEq a b c = (sol (+), sol (-))
  where
    sol f = (- b `f` sqrt (b ** 2 - 4 * a * c)) / 2 / a

thrustFromAngle :: Double -> Int -> Int -> Int -> Int
thrustFromAngle coef minA criticalAngle a = round $ coef / (a' - c) + d
  where
    ca :: Double
    ca = fromIntegral criticalAngle
    ma :: Double
    ma = fromIntegral minA
    a' :: Double
    a' = fromIntegral . abs $ a
    c = fst $ sqEq (100 - ma) (- (100 - ma) * (180 + ca)) (180 * ca * (100 - ma) - 180 * coef + ca * coef)
    d = 100 - coef / (ca - c)

betterTarget' :: Int -> Point -> Point -> Point
betterTarget' radius me destination
  | me `distance` destination < radius = me
  | otherwise = Point (pX me + round (dx - ddx')) (pY me + round (dy - ddy'))
  where
    radius' :: Double
    radius' = fromIntegral radius
    dx = fromIntegral $ pX destination - pX me
    dy = fromIntegral $ pY destination - pY me
    dist = sqrt $ dx * dx + dy * dy
    ddx' = radius' * dx / dist
    ddy' = radius' * dy / dist

betterTarget :: Int -> Point -> Point -> Point
betterTarget angle me destination
  | abs angle < 90 = trace (show ("<90", me, destination, angle)) $ rot angle
  | abs angle == 90 = trace (show ("=90", me, destination, angle)) $ rot (- angle + signum angle * 135)
  | otherwise = trace (show (">90", me, destination, angle)) $ rot (180 - angle)
  where
    dx :: Double
    dx = fromIntegral $ pX destination - pX me
    dy = fromIntegral $ pY destination - pY me
    rot :: Int -> Point
    rot a =
      Point
        (round $ (fromIntegral . pX) destination + dx * cos a' - dy * sin a')
        (round $ (fromIntegral . pY) destination + dx * sin a' + dy * cos a')
      where
        a' = fromIntegral a * pi / 180

addNextCheckpoint :: Point -> Point -> State Bot ()
addNextCheckpoint me p = do
  bot <- get
  when ((null . botCheckPoints) bot || (head . botCheckPoints) bot /= p) $ do
    let lap
          | (null . botCheckPoints) bot = 1
          | (head . botCheckPoints) bot `distance` (last . botCheckPoints) bot < 600 = botLap bot + 1
          | otherwise = botLap bot
        nearToFinish = botLap bot == 3 && p `distance` (last . botCheckPoints) bot < 600
        checkpoints = p : [me | null . botCheckPoints $ bot] <> botCheckPoints bot
    put bot {botCheckPoints = checkpoints, botIsNewCheckpoint = True, botLap = lap, botToFinish = nearToFinish}

setCheckpointIsNotNew :: State Bot ()
setCheckpointIsNotNew = modify (\bot -> bot {botIsNewCheckpoint = False})

thrustAndAngle :: Int -> (Int, Int, Int, Int)
thrustAndAngle dist
  | trace (show dist) $ dist > 5000 = (90, 50, 40, 50)
  | dist > 4000 = (70, 70, 30, 35)
  | dist > 3000 = (50, 70, 15, 25)
  | dist > 2000 = (35, 70, 8, 20)
  | otherwise = (30, 90, 5, 10)

updateBotParameters :: Input -> State Bot ()
updateBotParameters input = do
  bot <- get
  let (minThurst, criticalAngle, closeAngle, closeMinThrust)
        | botIsNewCheckpoint bot = thrustAndAngle . iDistance $ input
        | otherwise = (botMinThrust bot, botCriticalAngle bot, botCloseAngle bot, botCloseMinThurst bot)
  let coef = (fromIntegral . iDistance) input / 10
  put
    bot
      { botCoef = coef,
        botCloseCoef = coef / 10,
        botMinThrust = minThurst,
        botCriticalAngle = criticalAngle,
        botCloseAngle = closeAngle,
        botCloseMinThurst = closeMinThrust
      }

step :: Input -> State Bot Command
step input = do
  addNextCheckpoint (iMe input) . iCheckpoint $ input
  updateBotParameters input
  bot <- get
  let me = iMe input
      nextPoint = iCheckpoint input
      target = betterTarget (iAngle input) me nextPoint
      distToTarget = me `distance` nextPoint
      mkCommand = Command (pX target) (pY target)
      (bot', command)
        | distToTarget < botCloseDistance bot && iAngle input > botCloseAngle bot =
          (bot, mkCommand . Thrust $ thrustFromAngle (botCloseCoef bot) (botCloseMinThurst bot) (botCloseAngle bot) (iAngle input))
        | (abs . iAngle) input > botCriticalAngle bot =
          (bot, mkCommand . Thrust $ thrustFromAngle (botCoef bot) (botMinThrust bot) (botCriticalAngle bot) (iAngle input))
        | (abs . iAngle) input > 1 = (bot, mkCommand . Thrust $ 100)
        | botHaveBoost bot && botToFinish bot && (abs . iAngle) input == 0 =
          (bot {botHaveBoost = False}, mkCommand Boost)
        | otherwise = (bot, mkCommand . Thrust $ 100)
  put bot'
  setCheckpointIsNotNew
  return command

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  -- Auto-generated code below aims at helping you parse
  -- the standard input according to the problem statement.

  -- game loop
  foldM_
    ( \bot i -> do
        input_line <- getLine
        let input = words input_line
        let x = read . head $ input :: Int
        let y = read (input !! 1) :: Int
        let nextcheckpointx = read (input !! 2) :: Int -- x position of the next check point
        let nextcheckpointy = read (input !! 3) :: Int -- y position of the next check point
        let nextcheckpointdist = read (input !! 4) :: Int -- distance to the next checkpoint
        let nextcheckpointangle = read (input !! 5) :: Int -- angle between your pod orientation and the direction of the next checkpoint
        input_line1 <- getLine
        let input1 = words input_line1
        let opponentx = read . head $ input1 :: Int
        let opponenty = read (input1 !! 1) :: Int

        let inp =
              Input
                (Point x y)
                (Point nextcheckpointx nextcheckpointy)
                nextcheckpointdist
                nextcheckpointangle
                (Point opponentx opponenty)
                i
            (command, bot') = runState (step inp) bot
        print command
        return bot'
    )
    mkBot
    [0 ..]
