module Main where

import           Control.Monad

oneaway :: String -> String -> Bool
oneaway "" "" = True
oneaway "" cs2 = null . tail $ cs2
oneaway cs1 "" = null . tail $ cs1
oneaway (c1:cs1) (c2:cs2)
    | c1 == c2 = oneaway cs1 cs2
    | otherwise = cs1 == (c2:cs2) || (c1:cs1) == cs2 || cs1 == cs2

main :: IO ()
main = do
    let tests = [ ("pale", "ple", True)
                , ("pales", "pale", True)
                , ("pale", "bale", True)
                , ("pale", "bake", False)
                ]
    forM_ tests $ \(cs1, cs2, r) ->  print $ oneaway cs1 cs2 == r
