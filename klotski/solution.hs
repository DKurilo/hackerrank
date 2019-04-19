{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.Set (Set, empty, notMember, member, insert, singleton, size)
import Data.List (sort, foldl')
import Debug.Trace
import System.IO

type Point = (Int, Int)

addP ∷ Point → Point → Point
addP (y1,x1) (y2,x2) = (y1+y2,x1+x2)

moveP ∷ Point → Point → Point
moveP (y1,x1) (y2,x2) = (y2-y1,x2-x1)

data Turn = T String Point Point
    deriving (Eq)
instance Show Turn where
    show (T c p1 p2) = c ⧺ (' ':show p1) ⧺ (' ':show p2)

data Block = B String String Point [Point]
    deriving (Show, Eq, Ord)

getP ∷ Block → [Point]
getP (B _ _ tl ps) = map (addP tl) ps

getP' ∷ Block → [Point]
getP' (B _ _ _ ps) = ps

getC ∷ Block → String
getC (B c _ _ _) = c

getC' ∷ Block → String
getC' (B _ c' _ _) = c'

getTL ∷ Block → Point
getTL (B _ _ tl _) = tl

cBlock ∷ Block → Block
cBlock (B _ c tl ps) = B c c tl ps

isOverlapped ∷ Block → Block → Bool
isOverlapped b1 b2 = null [p1 ≡ p2 | p1 ← getP b1, p2 ← getP b2]

data Board = BD Int Int [Block]
    deriving (Show, Eq, Ord)

type Cache = Set Board

cBoard ∷ Board → Board
cBoard (BD h w bs) = BD h w $ sort ∘ map cBlock $ bs

setCacheable ∷ String → Board → Board
setCacheable c (BD h w bs) = BD h w $ go ∘ map (\(B c c' tl ps) → B c c' tl $ sort $ ps) $ bs
    where go ∷ [Block] → [Block]
          go [] = []
          go (b:bs)
              | c' ≠ getC b ∨ getC b ≡ c = b:go bs
              | otherwise = b:(go $ map (\(B c c'' tl ps) → if ps' ≡ ps
                                                              then B c c' tl ps
                                                              else B c c'' tl ps) bs)
              where ps' = getP' b
                    c' = getC' b

unsafePopB ∷ String → Board → (Block, Board)
unsafePopB c (BD h w bs) = (b, BD h w cbs)
    where (b, cbs) = get bs
          get [] = (B "!" "!" (0,0) [],[]) -- error
          get (b':bs')
              | getC b' ≡ c = (b', bs')
              | otherwise = (\(b'', bs'') → (b'', b':bs'')) $ get bs'

unsafePushB ∷ Block → Board → Board
unsafePushB b (BD h w bs) = BD h w (b:bs)

isFree ∷ Board → Block → Bool
isFree bd b = and ∘ map (\p → isInside bd p ∧ isEmpty bd p) ∘ getP $ b

isInside ∷ Board → Point → Bool
isInside (BD h w _) (y,x) = y ≥ 0 ∧ x ≥ 0 ∧ y < h ∧ x < w

isEmpty ∷ Board → Point → Bool
isEmpty (BD _ _ bs) p = go ∘ join ∘ map getP $ bs
    where go ∷ [Point] → Bool
          go [] = True
          go (p':ps)
              | p' ≡ p = False
              | otherwise = go ps

isUnique ∷ Board → Cache → Bool
isUnique bd cc = notMember (cBoard bd) cc

near ∷ Point → [Point]
near (y,x) = [(y - 1, x), (y, x + 1), (y + 1, x), (y, x - 1)]

place ∷ Block → Board → Cache → Maybe (Board, Cache, Bool)
place b bd cc
    | free ∧ isUnique bd' cc = Just (bd', insert (cBoard bd') cc, True)
    | free = Just (bd, cc, False)
    | otherwise = Nothing
    where bd' = unsafePushB b bd
          free = isFree bd b

turns ∷ (Board, [Turn]) → Cache → [String] → ([(Board, [Turn])], Cache)
turns (bd, ts) cc cs = foldl' (
      \(bdts, cc') c → let (b, bd') = unsafePopB c bd in
                     (\(bdts', cc'', _) → (bdts' ⧺ bdts, cc'')) $
                     movements bd' ts b (getTL b) cc' empty) ([],cc) cs

movements ∷ Board → [Turn] → Block → Point → Cache → Set Point →
            ([(Board, [Turn])], Cache, Set Point)
movements bd ts (B c c' tl ps) from cc cp = foldl' (
      \(bdts,cc',cp') p →
          if member p cp'
            then (bdts, cc',cp')
            else case place (B c c' p ps) bd cc' of
                Just (bd', cc'', True) →
                    let (bdts',cc''', cp'') =
                            movements bd ts (B c c' p ps) from cc'' $ insert p cp' in
                    (((bd', (T c from p):ts):bdts') ⧺ bdts, cc''', cp'')
                Just (bd', cc'', False) →
                    let (bdts',cc''', cp'') =
                            movements bd ts (B c c' p ps) from cc'' $ insert p cp' in
                    (bdts' ⧺ bdts, cc''', cp'')
                _ → (bdts, cc', cp')) ([],cc,cp) (near tl)

solve ∷ [[String]] → String → Point → [Turn]
solve t c p = snd $ go [(ibd,[])] (singleton ∘ cBoard $ ibd)
    where (ibd, cs) = parse t c
          go ∷ [(Board,[Turn])] → Cache → (Board, [Turn])
          go bdts cc = case final bdts of
              Just bdt → bdt
              _ → go nbdts ncc
              where (nbdts, ncc) = foldl' (\(bdts', cc') bdt → 
                             (\(bdts'', cc'') → (bdts'' ⧺ bdts', cc'')) $ turns bdt cc' cs)
                        ([],cc) bdts
          final ∷ [(Board,[Turn])] → Maybe (Board, [Turn])
          final [] = Nothing
          final ((bd, ts): bdts)
              | p' ≡ p = Just (bd, ts)
              | otherwise = final bdts
              where (B _ _ p' _, _) = unsafePopB c bd

updatetl ∷ Block → Block
updatetl (B c c' _ ps) = B c c' tl $ map (moveP tl) ps
    where tl = gettl ps

gettl ∷ [Point] → Point
gettl ps = (minimum ∘ map fst $ ps, minimum ∘ map snd $ ps)

parse ∷ [[String]] → String → (Board, [String])
parse css cm = (setCacheable cm $ BD (length css) (length ∘ head $ css) bs, map getC bs)
    where bs = map updatetl $ go css 0 []
          go ∷ [[String]] → Int → [Block] → [Block]
          go [] _ bs = bs
          go (cs: css) y bs = go css (y + 1) $ gox cs y 0 bs
          gox ∷ [String] → Int → Int → [Block] → [Block]
          gox [] _ _ bs = bs
          gox (c:cs) y x bs
              | head c ≡ '.' = gox cs y (x + 1) bs
              | otherwise = gox cs y (x + 1) $ insertB c (y,x) bs
          insertB ∷ String → Point → [Block] → [Block]
          insertB c p [] = [B c c (0,0) [p]]
          insertB c p ((B c' c'' tl ps):bs)
              | c ≡ c' = (B c c tl (p:ps):bs)
              | otherwise = (B c' c' tl ps):insertB c p bs

pack ∷ [Turn] → [Turn]
pack [] = []
pack (t:[]) = [t]
pack ((T c1 p11 p12):(T c2 p21 p22):ts)
    | c1 ≡ c2 = pack ((T c1 p21 p12):ts)
    | otherwise = (T c1 p11 p12): pack ((T c2 p21 p22):ts)

main ∷ IO()
main = do
    let getInt bx = case BSC.readInt bx of
                        Just (x,_) → x
                        _ → 0

    let getInts = map getInt <$> BSC.split ' '

    (n:m:_) ← getInts <$> BSC.getLine
    t ← forM [1..n] $ \_ → map BSC.unpack ∘ BSC.split ' ' <$> BSC.getLine
    c ← BSC.unpack <$> BSC.getLine
    (y:x:_) ← getInts <$> BSC.getLine

    let ts = reverse ∘ pack $ solve t c (y,x)
    BSC.putStrLn ∘ BSC.pack ∘ show ∘ length $ ts
    forM_ ts (BSC.putStrLn ∘ BSC.pack ∘ show)

