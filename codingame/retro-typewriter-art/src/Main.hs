import System.IO
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.ParserCombinators.ReadPrec as RPc
import Text.Read

data Recipe = Recipe {rCount :: Int, rChar :: Char}

instance Show Recipe where
  show r = replicate (rCount r) (rChar r)

instance Read Recipe where
  readPrec =
    ( do
        n <- readPrec
        return $ Recipe (if n < 10 then 1 else n `div` 10) (head . show $ n `mod` 10)
    )
      RPc.+++ ( do
                  n <- readPrec RPc.<++ return 1
                  _ <- lift . RP.string $ "sp"
                  return $ Recipe n ' '
              )
      RPc.+++ ( do
                  n <- readPrec RPc.<++ return 1
                  _ <- lift . RP.string $ "bS"
                  return $ Recipe n '\\'
              )
      RPc.+++ ( do
                  n <- readPrec RPc.<++ return 1
                  _ <- lift . RP.string $ "sQ"
                  return $ Recipe n '\''
              )
      RPc.+++ ( do
                  n <- readPrec RPc.<++ return 1
                  _ <- lift . RP.string $ "nl"
                  return $ Recipe n '\n'
              )
      RPc.+++ ( do
                  n <- readPrec RPc.<++ return 1
                  Recipe n <$> lift RP.get
              )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  getLine >>= putStrLn . concatMap (show . (read :: String -> Recipe)) . words
