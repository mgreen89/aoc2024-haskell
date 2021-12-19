module AoC.Challenge.Day19
  ( day19a
  , day19b
  ) where

import           AoC.Solution
import           Control.Monad                  ( (<=<)
                                                , guard
                                                )
import           Data.Foldable                  ( find )
import           Data.List                      ( transpose )
import           Data.List.Split                ( splitOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Traversable               ( sequenceA )
import           Linear.V3                      ( V3(..) )
import           Text.Read                      ( readEither )

import           Debug.Trace

type Beacon = V3 Int
type Scanner = V3 Int

listTo3Tuple :: [a] -> Either String (a, a, a)
listTo3Tuple [a, b, c] = Right (a, b, c)
listTo3Tuple _         = Left "Not a 2-elem list"

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

parseBeacon :: String -> Either String Beacon
parseBeacon =
  fmap (uncurry3 V3) . listTo3Tuple <=< traverse readEither . splitOn ","

parseScanner :: String -> Either String (Int, Set Beacon)
parseScanner s = do
  let (scannerLine : beaconLines) = lines s
  scannerNo <- readEither . (!! 2) . words $ scannerLine
  beacons   <- traverse parseBeacon beaconLines
  pure (scannerNo, S.fromList beacons)

parse :: String -> Either String (Map Int (Set Beacon))
parse = fmap M.fromList . traverse parseScanner . splitOn "\n\n"

-- Anti-clockwise rotations
rotX :: Beacon -> Beacon
rotX (V3 x y z) = V3 x z (-y)

rotY :: Beacon -> Beacon
rotY (V3 x y z) = V3 (-z) y x

rotZ :: Beacon -> Beacon
rotZ (V3 x y z) = V3 y (-x) z

-- Get all the possible orientations.
orientations :: Beacon -> [Beacon]
orientations = rots <=< initials
 where
  rots :: (Beacon, Beacon -> Beacon) -> [Beacon]
  rots (b, r) = take 4 . iterate r $ b

  initials :: Beacon -> [(Beacon, Beacon -> Beacon)]
  initials beacon =
    [ (beacon                     , rotX)
    , (rotY beacon                , rotZ)
    , (rotY . rotY $ beacon       , rotX)
    , (rotY . rotY . rotY $ beacon, rotZ)
    , (rotZ beacon                , rotY)
    , (rotZ . rotZ . rotZ $ beacon, rotY)
    ]

scannerOrientations :: Set Beacon -> [Set Beacon]
scannerOrientations =
  fmap S.fromList . transpose . fmap orientations . S.toList

{-
   For a pair of scanners, s and s', don't fiddle with s at all and
   try s' in all 24 oritentations.

   For each pair of points, p and p', where p is from s and p' is from s',
   if they are really the same beacon, s' is offset from s by p' - p.
   Therefore translate all beacons in s' (in the current orientation)
   by p - p', and check if there are at least 12 overlapping points
   in s and s'. If so, we've found the relative location of s and s'!

   Start with scanner 0 as s and try each other scanner until one is found.
   Then, combining all the scanners with known location into s, try all the
   scanners with 'unknown' location until an overlap is found.

   Do this until all the scanners have been moved into s, and therefore
   we know how many beacons there actually are, with their locations
   relative to scanner 0.
-}

-- | Find the first element in a list that returns a 'Just' value when
--   the operation returns.
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust op []      = Nothing
firstJust op (a : b) = case op a of
  Just x  -> Just x
  Nothing -> firstJust op b

-- Try merging s and s'
-- If a merge was successful, return the new set of placed beacons, and
-- the relative position of the matched scanner.
try :: Set Beacon -> Set Beacon -> Maybe (Set Beacon, Scanner)
try s s' =
  firstJust id
    . S.foldr (\p l -> S.foldr (\p' l' -> trySingle p p' : l') l s') []
    $ s
 where
  trySingle :: Beacon -> Beacon -> Maybe (Set Beacon, Scanner)
  trySingle p p' = do
    let delta = p - p'
        trys' = S.map (+ delta) s'
    guard $ S.size (S.intersection s trys') >= 12
    pure (S.union s trys', negate delta)

search
  :: Set Beacon
  -> Map Int Scanner
  -> Map Int (Set Beacon)
  -> Maybe (Set Beacon, Map Int Scanner, Map Int (Set Beacon))
search s scanners unplaced =
  firstJust id
    . foldr attempt []
    . concatMap (\(k, bs) -> fmap (k, ) (scannerOrientations bs))
    . M.toList
    $ unplaced
 where
  attempt
    :: (Int, Set Beacon)
    -> [Maybe (Set Beacon, Map Int Scanner, Map Int (Set Beacon))]
    -> [Maybe (Set Beacon, Map Int Scanner, Map Int (Set Beacon))]
  attempt (s'Id, s') l = case try s s' of
    Just (newS, scanPos) ->
      Just (newS, M.insert s'Id scanPos scanners, M.delete s'Id unplaced) : l
    Nothing -> Nothing : l

partARecurse
  :: Set Beacon -> Map Int Scanner -> Map Int (Set Beacon) -> Maybe (Set Beacon, Map Int Scanner)
partARecurse bs ss m = case search bs ss m of
  Just (bs', ss', m') ->
    if M.size m' == 0 then Just (bs', ss') else partARecurse bs' ss' m'
  Nothing -> Nothing

partA :: Map Int (Set Beacon) -> Either String Int
partA beacons =
  let s        = beacons M.! 0
      unplaced = M.delete 0 beacons
      scanners = M.singleton 0 (V3 0 0 0)
  in  case partARecurse s scanners unplaced of
        Just (bs, ss) -> Right $ S.size bs
        Nothing       -> Left "Something went wrong..."

manhattan :: V3 Int -> V3 Int -> Int
manhattan a b = sum . fmap abs $ (b - a)

partB :: Map Int (Set Beacon) -> Either String Int
partB beacons =
  let s        = beacons M.! 0
      unplaced = M.delete 0 beacons
      scanners = M.singleton 0 (V3 0 0 0)
  in  case partARecurse s scanners unplaced of
        Just (bs, ss) -> Right $
          maximum [ manhattan x y | x <- M.elems ss, y <- M.elems ss ]
        Nothing -> Left "Something went wrong..."

day19a :: Solution (Map Int (Set Beacon)) Int
day19a = Solution { sParse = parse, sShow = show, sSolve = partA }


day19b :: Solution (Map Int (Set Beacon)) Int
day19b = Solution { sParse = parse, sShow = show, sSolve = partB }
