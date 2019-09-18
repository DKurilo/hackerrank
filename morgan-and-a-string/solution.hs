{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
-- Not sure if it's the way, author wanted to solve it
-- Actually, I think I have to add hashes and couple of structures that allow me to pick
-- proper next element...
-- But I'm too lazy and it works.
-- UPD. Well.. I checked other solutions and can see this exercise is not "Expert" at all.
-- I was just able to solve it as is without thinking about any optimization.
-- So, yes I solve it, but don't use my solution. :)
-- Here is good solution:
-- https://www.hackerrank.com/rest/contests/master/challenges/morgan-and-a-string/hackers/kenanbit/download_solution
-- we need to add some unused character to each string and then:
-- solve :: (String, String) -> String
-- solve (a, "") = a
-- solve ("", b) = b
-- solve (a, b)
--     | a < b     = let (f,s) = splitHead a in f ++ solve (s,b)
--     | otherwise = let (f,s) = splitHead b in f ++ solve (a,s)

import Prelude.Unicode -- so no we can use ←, ∷, etc
import Control.Monad.Unicode -- so now we can use ≫, ≫=, etc.
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.Environment
import System.IO

morganAndString ∷ String → String → String
morganAndString "" cs = cs
morganAndString cs "" = cs
morganAndString (c1:cs1) (c2:cs2)
    | c1 > c2 = c2:morganAndString (c1:cs1) cs2
    | c1 < c2 = c1:morganAndString cs1 (c2:cs2)
    | null cs1 = c1:morganAndString [c1] cs2
    | null cs2 = c1:morganAndString [c1] cs1
    | otherwise = choosen
    where (dn1, dc1) = firstDiff c1 cs1
          (dn2, dc2) = firstDiff c2 cs2
          choosen
              | dc1 ≡ c1 ∧ dc2 ≡ c1 = (c1:cs1) ++ (c2:cs2)
              | dc1 ≡ c1 ∧ dc2 > c1 = replicate (dn1 + 1) c1 ++
                                      morganAndString (drop (dn1 + 1) cs1) (c2:cs2)
              | dc1 < c1 ∧ dc2 > c1 = replicate (dn1 + 1) c1 ++ [dc1] ++
                                      morganAndString (drop (dn1 + 1) cs1) (c2:cs2)
              | dc1 > c1 ∧ dc2 ≡ c1 = replicate (dn2 + 1) c1 ++
                                      morganAndString (c1:cs1) (drop (dn2 + 1) cs2)
              | dc1 > c1 ∧ dc2 < c1 = replicate (dn2 + 1) c1 ++ [dc2] ++
                                      morganAndString (c1:cs1) (drop (dn2 + 1) cs2)
              | dc1 < c1 ∧ dc2 < c1 = choosenSmall
              | dc1 > c1 ∧ dc2 > dc1 = replicate (dn1 + dn2 + 2) c1 ++ [dc1] ++
                                       morganAndString (drop (dn1 + 1) cs1) (drop dn2 cs2)
              | dc2 > c1 ∧ dc1 > dc2 = replicate (dn1 + dn2 + 2) c1 ++ [dc2] ++
                                       morganAndString (drop dn1 cs1) (drop (dn2 + 1) cs2)
              | otherwise = replicate (dn1 + dn2 + 2) c1 ++ [dc1] ++
                            min (morganAndString (drop (dn1 + 1) cs1) (drop dn2 cs2))
                                (morganAndString (drop dn1 cs1) (drop (dn2 + 1) cs2))
          choosenSmall
              | dn1 < dn2 ∨ (dn1 ≡ dn2 && dc1 < dc2) = replicate (dn1 + 1) c1 ++ [dc1] ++
                                                   morganAndString (drop (dn1 + 1) cs1) (c2:cs2)
              | dn1 > dn2 ∨ (dn1 ≡ dn2 && dc2 < dc1) = replicate (dn2 + 1) c1 ++ [dc2] ++
                                                   morganAndString (c1:cs1) (drop (dn2 + 1) cs2)
              | otherwise = replicate (dn1 + 1) c1 ++ [dc1] ++
                            min (morganAndString (drop (dn1 + 1) cs1) (c2:cs2))
                                (morganAndString (c1:cs1) (drop (dn2 + 1) cs2))

firstDiff ∷ Char → String → (Int, Char)
firstDiff = go 0
    where go n c [] = (n, c)
          go n c (c':cs)
              | c ≡ c' = go (n + 1) c cs
              | otherwise = (n, c')

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    q ← getInt <$> BSC.getLine
    forM_ [1..q] $ \_ → do
        cs1 ← BSC.unpack <$> BSC.getLine
        cs2 ← BSC.unpack <$> BSC.getLine
        -- BSC.putStrLn ∘
        BSC.hPutStrLn fptr ∘
            BSC.pack $ morganAndString cs1 cs2

    hFlush fptr
    hClose fptr
