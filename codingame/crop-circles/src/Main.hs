import Data.Char (ord)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as S
import System.IO
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.ParserCombinators.ReadPrec as RC
import Text.Read

data Coord = Coord {cX :: Int, cY :: Int} deriving (Eq, Ord)

charToInt :: Char -> Int
charToInt c = ord c - ord 'a'

data Circle = Circle {ciC :: Coord, ciR :: Int}

instance Read Circle where
  readPrec = do
    x <- charToInt <$> lift (RP.satisfy (\c -> c >= 'a' && c <= 's'))
    y <- charToInt <$> lift (RP.satisfy (\c -> c >= 'a' && c <= 'y'))
    Circle (Coord x y) <$> readPrec

data Instruction = Plant Circle | Mow Circle | PlantMow Circle

instance Read Instruction where
  readPrec =
    ( do
        _ <- (lift . RP.string) "PLANTMOW"
        PlantMow <$> readPrec
    )
      RC.<++ ( do
                 _ <- (lift . RP.string) "PLANT"
                 Plant <$> readPrec
             )
      RC.<++ (Mow <$> readPrec)

newtype Field = Field {unField :: Set Coord}

instance Show Field where
  show (Field field) =
    intercalate
      "\n"
      [ intercalate
          ""
          [ if Coord x y `S.member` field then "  " else "{}" | x <- [0 .. 18]
          ]
        | y <- [0 .. 24]
      ]

isOnCircle :: Coord -> Circle -> Bool
isOnCircle c cir = 4 * (diffx * diffx + diffy * diffy) <= ciR cir * ciR cir
  where
    diffx = cX c - (cX . ciC) cir
    diffy = cY c - (cY . ciC) cir

circle :: Circle -> [Coord]
circle cir = [c | x <- [xMin .. xMax], y <- [yMin .. yMax], let c = Coord x y, isOnCircle c cir]
  where
    xMin = max 0 ((cX . ciC) cir - ciR cir)
    xMax = min 18 ((cX . ciC) cir + ciR cir)
    yMin = max 0 ((cY . ciC) cir - ciR cir)
    yMax = min 24 ((cY . ciC) cir + ciR cir)

applyInstruction :: Field -> Instruction -> Field
applyInstruction f (Mow cir) = Field $ unField f `S.union` (S.fromList . circle) cir
applyInstruction f (PlantMow cir) =
  Field
    . foldl (\f' c -> (if c `S.member` f' then S.delete else S.insert) c f') (unField f)
    . circle
    $ cir
applyInstruction f (Plant cir) = Field $ unField f S.\\ (S.fromList . circle) cir

main :: IO ()
main = hSetBuffering stdout NoBuffering >> (foldl applyInstruction (Field S.empty) . map read . words <$> getLine) >>= print
