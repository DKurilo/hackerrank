{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import System.IO
import Data.Maybe
import qualified Data.List as DL
import Data.List.Split
import Control.Monad
import System.IO

import Debug.Trace

class Queue a where
    put ∷ a b → b → a b
    pop ∷ a b → a b
    peek ∷ a b → Maybe b

data FIFO a = FIFOEmpty | FIFO [a] [a]
    deriving (Eq, Show)
instance Queue FIFO where
    put FIFOEmpty a = FIFO [] [a]
    put (FIFO as []) a = FIFO [a] $ reverse as
    put (FIFO as ras) a = FIFO (a:as) ras
    pop FIFOEmpty = FIFOEmpty
    pop (FIFO (_:[]) []) = FIFOEmpty
    pop (FIFO as []) = FIFO [] $ tail ∘ reverse $ as
    pop (FIFO [] (_:[])) = FIFOEmpty
    pop (FIFO as (ra:[])) = FIFO [] $ reverse as
    pop (FIFO as ras) = FIFO as $ tail ras
    peek FIFOEmpty = Nothing
    peek (FIFO _ (ra:_)) = Just ra

data LIFO a = LIFOEmpty | LIFO [a]
    deriving (Eq, Show)
instance Queue LIFO where
    put LIFOEmpty a = LIFO [a]
    put (LIFO as) a = LIFO (a:as)
    pop LIFOEmpty = LIFOEmpty
    pop (LIFO (_:[])) = LIFOEmpty
    pop (LIFO (a:as)) = LIFO as
    peek LIFOEmpty = Nothing
    peek (LIFO (a:as)) = Just a

data Action = Append Int String | Delete Int | Print Int | Undo | Noop
    deriving (Show, Eq)

data Editor = ED String Int (LIFO Action) [String]
    deriving (Eq)
instance Show Editor where
    show (ED _ _ _ o) = DL.intercalate "\n" ∘ reverse $ o

action ∷ Action → Editor → Editor
action (Append sl s) (ED c cl buf o) = ED (s⧺c) (cl+sl) (put buf (Delete sl)) o
action (Delete l) (ED c cl buf o)
    | l ≤ cl = ED (drop l c) (cl-l) (put buf (Append l $ take l c)) o
    | l > cl = ED (drop cl c) 0 (put buf (Append cl c)) o
action (Print k) (ED [] 0 buf o) = ED [] 0 buf o
action (Print k) (ED c cl buf o) = ED c cl buf ([c!!(cl-k)]:o)
action Undo (ED c cl buf o) = case peek buf of
    Just a → removeLastAction ∘ action a $ ED c cl (pop buf) o
    Nothing → ED c cl buf o

removeLastAction ∷ Editor → Editor
removeLastAction (ED c cl buf o) = ED c cl (pop buf) o

process ∷ Editor → [Action] → Editor
process ed [] = ed
process ed (a:as) = process (action a ed) as

s2a ∷ [String] → Action
s2a ("1":s:_) = Append (length s) (reverse s)
s2a ("2":s:_) = Delete $ (read s ∷ Int)
s2a ("3":s:_) = Print $ (read s ∷ Int)
s2a ("4":_)   = Undo

main ∷ IO()
main = do
    t ← readLn ∷ IO Int
    
    as ← forM [1..t] $ \_ → do
        css <- getLine
        return ∘ s2a ∘ splitOn " " $ css

    putStrLn ∘ show ∘ process (ED "" 0 LIFOEmpty []) $ as

