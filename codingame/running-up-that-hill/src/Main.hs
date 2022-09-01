{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad
import Data.Foldable (toList)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import System.IO
import Text.ParserCombinators.ReadP as RP
import Text.Read

abc :: String
abc = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:"

modulo :: Int
modulo = length abc

newtype Dummy = Dummy String deriving (Show)

instance Read Dummy where
  readPrec = Dummy <$> lift (RP.many RP.get)

newtype HillChar = HillChar {unHillChar :: Int} deriving (Eq)

instance Read HillChar where
  readPrec = lift $ do
    c <- RP.get
    case c `elemIndex` abc of
      Just n -> return . HillChar $ n
      _ -> RP.pfail

instance Show HillChar where
  show (HillChar n) = [abc !! n]

newtype HillText = HillText {unHillText :: Seq HillChar} deriving (Eq)

instance Read HillText where
  readPrec = HillText . S.fromList <$> (lift . RP.many) (readPrec_to_P readPrec 0)

parseHillText :: String -> HillText
parseHillText = HillText . S.fromList . map (read . (: []))

instance Show HillText where
  show = concatMap show . toList . unHillText

type Matrix = Seq (Seq Int)

diag :: Int -> Matrix
diag n =
  S.fromList
    [ S.fromList $ replicate j (0 :: Int) <> [1] <> replicate (n - 1 - j) 0
      | j <- [0 .. n - 1]
    ]

-- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
inverse :: Int -> Int -> Maybe Int
inverse a m = go 0 1 m a
  where
    go t t' r r'
      | r' == 0 && r > 1 = Nothing -- not invertable
      | r' == 0 && t < 0 = Just $ t + m
      | r' == 0 = Just t
      | otherwise = go t' (t - q * t') r' (r - q * r')
      where
        q = r `div` r'

augment :: Matrix -> Matrix -> Matrix
augment = S.zipWith (S.><)

unAugment :: Matrix -> Matrix
unAugment m = fmap (S.drop n) m
  where
    n = S.length m

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

solveEq :: Matrix -> Matrix -> Maybe Matrix
solveEq equations spare =
  makeDiag . fst3
    <$> ( head
            . filter
              ( \case
                  Just (_, m, _) -> S.null m
                  _ -> True
              )
            . scanl (\mbMUM i -> (\(newM, m, sp) -> addLine i newM m sp) =<< mbMUM) (Just (S.empty, equations, spare))
        )
      [0 ..]
  where
    addLine :: Int -> Matrix -> Matrix -> Matrix -> Maybe (Matrix, Matrix, Matrix)
    addLine i newM m sp = case findInvertable i m of
      Just k -> Just (newM S.|> newLine, applyLine newLine i (S.deleteAt k m), sp)
        where
          newLine = processLine k m i
      _ -> case findReducable i m of
        Just k
          | needParam ->
            Just
              ( newM',
                applyLine newLine i (S.deleteAt k (addParam m S.|> spareLine)),
                S.drop 1 sp
              )
          | otherwise -> Just (newM S.|> newLine, applyLine newLine i (S.deleteAt k m), sp)
          where
            (newLine, needParam) = reduceLine k m newM i
            addParam = fmap (S.insertAt n 0)
            newM' = addParam newM S.|> newLine
            spareLine = S.foldlWithIndex (\spL j l -> applyLine l j (S.singleton spL) `S.index` 0) (S.insertAt n 0 $ sp `S.index` 0) newM'
        _ -> case findComposable i m of
          Just (j, k) -> addLine i newM (S.update j newLine m) sp
            where
              newLine = composeLines j k m
          _ -> case findReady i m of
            Just (k, j) -> addLine i newM (updateWithLine k j m) sp
            _ -> Nothing
      where
        n = S.length newM + S.length m
    findReady :: Int -> Matrix -> Maybe (Int, Int)
    findReady i m
      | S.null readyLines = Nothing
      | otherwise = Just $ readyLines `S.index` 0
      where
        n' = S.length m
        readyLines =
          fmap fst
            . S.filter snd
            . S.zipWith
              ( \j l ->
                  let nonZeroPos = S.length . S.takeWhileL (== 0) $ l
                   in ( (j, nonZeroPos),
                        nonZeroPos > i
                      )
              )
              (S.fromList [0 .. n' - 1])
            $ m
    updateWithLine :: Int -> Int -> Matrix -> Matrix
    updateWithLine k i m =
      S.update k line'
        . foldl
          ( \m' j ->
              let mul = m' `S.index` j `S.index` i
               in S.update j (S.zipWith (\x y -> (y - x * mul) `mod` modulo) line' (m `S.index` j)) m'
          )
          m
        $ [0 .. S.length m]
      where
        line = m `S.index` k
        invD = fromMaybe 0 . (`inverse` modulo) . (`S.index` i) $ line
        line' = fmap (\x -> (x * invD) `mod` modulo) line
    findInvertable :: Int -> Matrix -> Maybe Int
    findInvertable i m
      | S.null invertables = Nothing
      | otherwise = Just $ invertables `S.index` 0
      where
        n' = S.length m
        invertables =
          fmap fst
            . S.filter snd
            . S.zipWith (\j l -> (j, isJust (inverse (l `S.index` i) modulo))) (S.fromList [0 .. n' - 1])
            $ m
    applyLine :: Seq Int -> Int -> Matrix -> Matrix
    applyLine line i =
      fmap
        ( \l ->
            let mul = l `S.index` i
             in S.zipWith
                  (\j x -> (x - mul * (line `S.index` j)) `mod` modulo)
                  (S.fromList [0 .. S.length l - 1])
                  l
        )
    processLine :: Int -> Matrix -> Int -> Seq Int
    processLine k m i = fmap (\x -> x * mul `mod` modulo) line
      where
        line = m `S.index` k
        mul = fromMaybe 0 $ inverse (line `S.index` i) modulo
    findReducable :: Int -> Matrix -> Maybe Int
    findReducable i m
      | S.null reducables = Nothing
      | otherwise = Just $ reducables `S.index` 0
      where
        n' = S.length m
        reducables =
          fmap fst
            . S.filter snd
            . S.zipWith (\j l -> (j, all (\x -> x `mod` gcd (l `S.index` i) modulo == 0) l)) (S.fromList [0 .. n' - 1])
            $ m
    reduceLine :: Int -> Matrix -> Matrix -> Int -> (Seq Int, Bool)
    reduceLine k m newM i
      | needToAddParam m || needToAddParam newM = (S.insertAt n modulo' line'', True)
      | otherwise = (line'', False)
      where
        n = S.length newM + S.length m
        line = m `S.index` k
        d = gcd (line `S.index` i) modulo
        line' = fmap (`div` d) line
        modulo' = modulo `div` d
        invD = fromMaybe 0 $ inverse (line' `S.index` i) modulo'
        applyInv = fmap (\x -> (x * invD) `mod` modulo')
        line'' = applyInv line'
        needToAddParam = any (\l -> (l `S.index` i) `mod` d /= 0)
    findComposable :: Int -> Matrix -> Maybe (Int, Int)
    findComposable i m
      | null compositions = Nothing
      | otherwise = Just $ head compositions
      where
        n' = S.length m
        compositions =
          [ (j, k)
            | j <- [0 .. n' - 2],
              k <- [j + 1 .. n' - 1],
              isJust . (`inverse` modulo) $ (((m `S.index` j `S.index` i) - (m `S.index` k `S.index` i)) `mod` modulo)
          ]
    composeLines :: Int -> Int -> Matrix -> Seq Int
    composeLines i j m = S.zipWith (\x y -> (x - y) `mod` modulo) l1 l2
      where
        l1 = m `S.index` i
        l2 = m `S.index` j
    makeDiag :: Matrix -> Matrix
    makeDiag m = foldl (\m' i -> applyLineBackward (m' `S.index` i) i m') m [n' - 1, n' -2 .. 1]
      where
        n' = S.length m
    applyLineBackward :: Seq Int -> Int -> Matrix -> Matrix
    applyLineBackward line i m =
      foldl
        ( \m' j ->
            let mul = m' `S.index` j `S.index` i
             in S.adjust' (S.zipWith (\k x -> (x - mul * (line `S.index` k)) `mod` modulo) (S.fromList [0 .. S.length line - 1])) j m'
        )
        m
        [i - 1, i - 2 .. 0]

restoreMatrix :: HillText -> HillText -> Maybe Matrix
restoreMatrix ciphered clear =
  head
    . filter (\m -> hillCipher m clear == ciphered)
    <$> ( sequence
            . filter isJust
            . map doer
            . filter (\x -> textLength `mod` x == 0)
            $ [maxMatrixSize, maxMatrixSize - 1 .. 2]
        )
  where
    textLength = S.length . unHillText $ ciphered
    maxMatrixSize = last . filter (\n -> textLength `mod` n == 0) . takeWhile (\n -> n * n <= textLength) $ [2 ..]
    doer :: Int -> Maybe Matrix
    doer n = S.fromList <$> sequence [S.take n . join . unAugment <$> ((`solveEq` spare i) . augment equation . res $ i) | i <- [0 .. n - 1]]
      where
        (chs, cipheredChs) =
          foldl
            ( \(chs', cipheredChs') (ch, cipheredCh) ->
                if ch `elem` chs'
                  then (chs', cipheredChs')
                  else (chs' S.|> ch, cipheredChs' S.|> cipheredCh)
            )
            (S.empty, S.empty)
            $ S.zip (S.chunksOf n . unHillText $ clear) (S.chunksOf n . unHillText $ ciphered)
        ciphered' = join cipheredChs
        equation =
          S.fromList
            [ S.fromList
                [ unHillChar (chs `S.index` i `S.index` j)
                  | j <- [0 .. n - 1]
                ]
              | i <- [0 .. n - 1]
            ]
        res l =
          S.fromList
            [ S.singleton . unHillChar . (`S.index` (l + i * n)) $ ciphered'
              | i <- [0 .. n - 1]
            ]
        spare i =
          augment
            (S.fromList [S.fromList [unHillChar $ chs `S.index` k `S.index` j | j <- [0 .. n - 1]] | k <- [n .. S.length chs - 1]])
            (fmap (S.singleton . unHillChar) . S.drop (n * n + i) $ ciphered')

inverseMatrix :: Matrix -> Maybe Matrix
inverseMatrix m = unAugment <$> ((`solveEq` S.empty) . augment m . diag $ n)
  where
    n = S.length m

hillDecipher :: Matrix -> HillText -> Maybe HillText
hillDecipher m ht = (`hillCipher` ht) <$> inverseMatrix m

hillCipher :: Matrix -> HillText -> HillText
hillCipher m ht = HillText . (cipherChunk =<<) $ chs
  where
    n = S.length m
    chs = S.chunksOf n . unHillText $ ht
    cipherChunk ch =
      fmap
        ( HillChar
            . (`mod` modulo)
            . sum
            . toList
            . S.zipWith (\(HillChar x) y -> x * y) ch
        )
        m

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  ciphered <- parseHillText <$> getLine
  clear <- parseHillText <$> getLine
  decipherMe <- parseHillText <$> getLine
  cipherMe <- parseHillText <$> getLine

  let matrix = fromMaybe S.empty . restoreMatrix ciphered $ clear

  print . fromMaybe (HillText S.empty) . hillDecipher matrix $ decipherMe
  print . hillCipher matrix $ cipherMe
