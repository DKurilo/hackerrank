{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.IO

-- Initial state:
--  1
-- 245
--  6
--  3
-- is D 1 2 4 5 6 3
data Dice = D Int Int Int Int Int Int
    deriving (Show, Eq, Ord)

dinit ∷ Dice
dinit = D 1 2 4 5 6 3

dr ∷ Dice → Dice
dr (D a1 a2 a4 a5 a6 a3) = D a3 a2 a1 a5 a4 a6

db ∷ Dice → Dice
db (D a1 a2 a4 a5 a6 a3) = D a5 a1 a4 a6 a2 a3

dval ∷ Dice → Int
dval (D a1 _ _ _ _ _) = a1

dicesum ∷ Int → Int → Int
dicesum m n = dicesum' m n dinit

dicesum' ∷ Int → Int → Dice → Int
dicesum' m n d
    | m ≡ 1 ∧ n ≡ 1 = dval d
    | m > 3 ∧ n > 3 ∧ (m + n) ≥ 4 = dval d + if b > r
                                               then dicesum' (m - 1) n b
                                               else dicesum' m (n - 1) r
    | m > 1 ∧ n > 1 = dval d + max (dicesum' (m - 1) n $ db d) (dicesum' m (n - 1) $ dr d)
    | m > 1 = dval d + (dicesum' (m - 1) n $ db d)
    | n > 1 = dval d + (dicesum' m (n - 1) $ dr d)
    where b = db d
          r = dr d

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    t ← getInt <$> BSC.getLine

    forM_ [1..t] $ \_ → (BSC.pack ∘ show ∘ (\(m:n:_) → dicesum m n) ∘ getInts <$> BSC.getLine)
                        ≫= BSC.putStrLn

