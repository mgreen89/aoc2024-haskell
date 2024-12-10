module AoC.Challenge.Day09 (
  day09a,
  day09b,
)
where

import AoC.Solution
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Safe (headMay)
import Text.Read (readEither)

parse :: String -> Either String [Int]
parse = traverse (readEither . pure)

toSeq :: [Int] -> Seq (Maybe Int)
toSeq = go 0 S.empty
 where
  go n a [x] = a <> S.replicate x (Just n)
  go n a (x : s : rest) = go (n + 1) (a <> S.replicate x (Just n) <> S.replicate s Nothing) rest
  go _ _ _ = error "Invalid input"

compact :: Seq (Maybe Int) -> Seq Int
compact = go S.empty
 where
  go pref (Just x S.:<| xs) = go (pref S.|> x) xs
  go pref (Nothing S.:<| xs) = case xs of
    xs' S.:|> Nothing -> go pref (Nothing S.<| xs')
    xs' S.:|> Just a -> go (pref S.|> a) xs'
    S.Empty -> pref
  go pref S.Empty = pref

chksum :: (Foldable f) => f Int -> Int
chksum = sum . fmap (uncurry (*)) . zip [0 ..] . toList

solveA :: [Int] -> Int
solveA = chksum . compact . toSeq

day09a :: Solution [Int] Int
day09a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}

toMaps :: [Int] -> (IntMap (Int, Int), IntMap Int)
toMaps = go 0 0 IM.empty IM.empty
 where
  go _ _ fs ss [] = (fs, ss)
  go pos fid fs ss [f] = (IM.insert pos (fid, f) fs, ss)
  go pos fid fs ss (f : s : rest) =
    go
      (pos + f + s)
      (fid + 1)
      (IM.insert pos (fid, f) fs)
      (IM.insert (pos + f) s ss)
      rest

compactFiles :: (IntMap (Int, Int), IntMap Int) -> Int
compactFiles (fs, ss) =
  fst $ IM.foldrWithKey go (0, ss) fs
 where
  go :: Int -> (Int, Int) -> (Int, IntMap Int) -> (Int, IntMap Int)
  go bid (fid, flen) (chk, spaces) = case findSpace spaces bid flen of
    Just (i, l) ->
      ( chk + (sum . fmap (* fid) . take flen $ [i ..])
      , IM.insert (i + flen) (l - flen) . IM.delete i $ spaces
      )
    Nothing -> (chk + (sum . fmap (* fid) . take flen $ [bid ..]), spaces)

  findSpace spaces fid flen =
    headMay
      . dropWhile ((< flen) . snd)
      . IM.toAscList
      . IM.takeWhileAntitone (< fid)
      $ spaces

solveB :: [Int] -> Int
solveB = compactFiles . toMaps

day09b :: Solution [Int] Int
day09b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB}
