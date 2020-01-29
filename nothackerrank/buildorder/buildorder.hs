module Main where

import           Control.Monad
import           Data.List     (foldl')
import qualified Data.Map      as M
import qualified Data.Set      as S

type Project = String

type Dependency = (String, String)

data Node a = Node (S.Set a) (S.Set a)

type Graph a =  M.Map a (Node a)

buildGraph :: [Project] -> [Dependency] -> Graph Project
buildGraph ps = foldl addDependency (M.fromList (zip  ps (repeat (Node S.empty S.empty))))

addDependency :: Graph Project -> Dependency -> Graph Project
addDependency g (p1, p2) = M.adjust (\(Node ps cs) -> Node ps (S.insert p2 cs)) p1 (M.adjust (\(Node ps cs) -> Node (S.insert p1 ps) cs) p2 g)

buildorder :: [Project] -> [Dependency] -> Maybe [Project]
buildorder ps = go . buildGraph ps
    where go :: Graph Project -> Maybe [Project]
          go g | M.null g = Just []
               | null readyToBuild = Nothing
               | otherwise = (readyToBuild ++) <$> go g'
              where readyToBuild = M.keys . M.filter (\(Node ps _) -> S.size ps == 0) $ g
                    g' = foldl' (\g'' p -> let (Just (Node ps cs)) = p `M.lookup` g'' in
                                           M.mapMaybeWithKey (\p' n@(Node ps' cs') -> if p' == p
                                                                                        then Nothing
                                                                                        else if p' `S.member` ps
                                                                                        then Just (Node ps' (S.delete p cs'))
                                                                                        else if p' `S.member` cs
                                                                                               then Just (Node (S.delete p ps') cs')
                                                                                               else Just n) g'') g readyToBuild

main :: IO ()
main = do
    let tests = [ (["a", "b", "c", "d", "e", "f"], [("a", "d"), ("f", "b"), ("b", "d"), ("f", "a"), ("d", "c")]) -- ["e","f","a","b","d","c"]
                , ([[c] | c <- "abcdefghijklm"], [ ("a", "d"), ("a", "e"), ("a", "f"), ("b", "f"), ("b", "g")
                                                 , ("c", "g"), ("c", "h"), ("d", "k"), ("e", "i"), ("f", "i")
                                                 , ("g", "j"), ("h", "l"), ("i", "k"), ("j", "k"), ("k", "l")
                                                 , ("l", "m")
                                                 ]) -- ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m"]
                , ([[c] | c <- "abcdefg"], [ ("f", "c"), ("f", "a"), ("f", "b"), ("c", "a"), ("b", "a")
                                           , ("b", "e"), ("a", "g")
                                           ]) -- ["f", "d", "c", "b", "g", "a", "e"]
                , ([show x | x <- [1..10000]], [])
                ]
    forM_ tests (print . uncurry  buildorder)
