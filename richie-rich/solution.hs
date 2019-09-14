{-# LANGUAGE OverloadedStrings, UnicodeSyntax, TupleSections #-}

module Main where

import Prelude.Unicode -- so no we can use ←, ∷, etc
import Control.Monad.Unicode -- so now we can use ≫, ≫=, etc.
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import System.Environment
import System.IO
import Data.Maybe

type PChar = (Char, Int)
type PString = ([PChar], String)
highestValuePalindrome ∷ String → Int → Int → String
highestValuePalindrome cs n k = maybe "-1" (uncurry improve) $ build csl csm csr k
    where csl = take (n `div` 2) cs
          csmr = drop (n `div` 2) cs
          csm = take (n `mod` 2) csmr
          csr = reverse ∘ drop (n `mod` 2) $ csmr

build ∷ String → String → String → Int → Maybe (PString, Int)
build [] _ (_:_) _ = Nothing
build (_:_) _ [] _ = Nothing
build csl csm csr 0
    | csl ≡ csr = Just ((map (,0) csl, csm), 0)
    | otherwise = Nothing
build [] csm [] n = Just (([], csm), n)
build (cl:csl) csm (cr:csr) n =
    (\((pcsl, csm), n') → (((max cl cr, c):pcsl,csm), n')) <$> build csl csm csr (n - c)
    where c = if cl ≡ cr then 0 else 1

improve ∷ PString → Int → String
improve pcs n = (\(csl, csm) → csl ++ csm ++ reverse csl) $ go pcs n
    where go (pcs, csm) 0 = (map fst pcs, csm)
          go ([], _:_) _ = ("", "9")
          go ([], "") _ = ("", "")
          go ((c, ct):pcs,csm) n
              | n ≥ 2 - ct ∧ c < '9' = ('9':cs', csm')
              | otherwise = (c:cs'', csm'')
              where (cs', csm') = go (pcs,csm) (n + ct - 2)
                    (cs'', csm'') = go (pcs,csm) n

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    stdout ← getEnv "OUTPUT_PATH"
    fptr ← openFile stdout WriteMode

    (n:k:_) ← getInts <$> BSC.getLine
    cs ← BSC.unpack <$> BSC.getLine

    BSC.putStrLn ∘ BSC.pack $ highestValuePalindrome cs n k
    -- BSC.hPutStrLn fptr ∘ BSC.pack $ highestValuePalindrome cs n k

    hFlush fptr
    hClose fptr
