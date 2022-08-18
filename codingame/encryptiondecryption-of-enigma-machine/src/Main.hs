import Control.Monad
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import System.IO

caesar :: [Int] -> String -> String
caesar ns cs =
  zipWith
    (\c n' -> abc !! (((fromMaybe 0 . elemIndex c) abc + l + n') `mod` l))
    cs
    ns
  where
    abc = ['A' .. 'Z']
    l = length abc

rotor :: String -> String -> String -> String
rotor rFrom rTo = map (\c -> rTo !! (fromMaybe 0 . elemIndex c) rFrom)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  operation <- getLine
  n <- read <$> getLine
  let r0 = ['A' .. 'Z']
  (r1 : r2 : r3 : _) <- replicateM 3 getLine
  getLine
    >>= putStrLn
      . ( if operation == "ENCODE"
            then rotor r0 r3 . rotor r0 r2 . rotor r0 r1 . caesar [n ..]
            else caesar [- n, (- n -1) ..] . rotor r1 r0 . rotor r2 r0 . rotor r3 r0
        )
