module AoC.Challenge.Day19
  ( day19a
  , day19b
  ) where

import           AoC.Solution
import           AoC.Util                       ( maybeToEither )
import           Control.Monad                  ( (<=<)
                                                , guard
                                                )
import           Data.List                      ( tails
                                                , transpose
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Linear.V3                      ( V3(..) )
import           Text.Read                      ( readEither )

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

-- Get all the possible orientations for a single beacon.
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

-- Get all the possible orientations for beacons from a single scanner.
scannerOrientations :: Set Beacon -> [Set Beacon]
scannerOrientations =
  fmap S.fromList . transpose . fmap orientations . S.toList

{-
   For a pair of scanners, s0 and s1, don't fiddle with s at all and
   try s1 in all 24 orientations.

   For each pair of points, p0 and p1, from s0 and s1 respectively,
   if they are really the same beacon, s1 is offset from s0 by p1 - p0.
   Therefore translate all beacons in s1 (in the current orientation)
   by p0 - p1, and check if there are at least 12 overlapping points
   in s0 and s1. If so, we've found the relative location of s1!

   Start with scanner 0 as s0 and try each orientation of each other scanner
   until one that overlaps is found. Then combine the beacons into the new
   s0 and continue trying to match the other scanners in the same way.

   Do this until all the scanners have been moved into s0.
-}

-- | Find the first element in a list that returns a 'Just' value when
--   applied to the given operation.
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _  []      = Nothing
firstJust op (a : b) = case op a of
  Just x  -> Just x
  Nothing -> firstJust op b

-- Try aligning s0 and s1 by checking if each pair could be the same beacon.
-- If alignment was successful, return the new set of placed beacons, and
-- the relative position of the matched scanner.
align :: Set Beacon -> Set Beacon -> Maybe (Set Beacon, Scanner)
align s0 s1 =
  firstJust id
    . S.foldr (\p0 l -> S.foldr (\p1 l' -> go p0 p1 : l') l s1) []
    $ s0
 where
  go :: Beacon -> Beacon -> Maybe (Set Beacon, Scanner)
  go p0 p1 = do
    let delta = p0 - p1
        s1'   = S.map (+ delta) s1
    guard $ S.size (S.intersection s0 s1') >= 12
    pure (S.union s0 s1', negate delta)

-- Merge one scanner from the set of unplaced scanners into s0, if possible.
mergeOne
  :: Set Beacon
  -> Map Int Scanner
  -> Map Int (Set Beacon)
  -> Maybe (Set Beacon, Map Int Scanner, Map Int (Set Beacon))
mergeOne s0 sPoss unplaced =
  firstJust id
    . foldr attemptMerge []
    . concatMap (\(k, s1) -> fmap (k, ) (scannerOrientations s1))
    . M.toList
    $ unplaced
 where
  attemptMerge
    :: (Int, Set Beacon)
    -> [Maybe (Set Beacon, Map Int Scanner, Map Int (Set Beacon))]
    -> [Maybe (Set Beacon, Map Int Scanner, Map Int (Set Beacon))]
  attemptMerge (s1id, s1) l = case align s0 s1 of
    Just (s0', sPos) ->
      Just (s0', M.insert s1id sPos sPoss, M.delete s1id unplaced) : l
    Nothing -> Nothing : l

mergeScanners
  :: Set Beacon
  -> Map Int Scanner
  -> Map Int (Set Beacon)
  -> Maybe (Set Beacon, Map Int Scanner)
mergeScanners s0 scanPos unmerged = case mergeOne s0 scanPos unmerged of
  Just (s0', scanPos', unmerged') -> if M.null unmerged'
    then Just (s0', scanPos')
    else mergeScanners s0' scanPos' unmerged'
  Nothing -> Nothing

runMerge :: Map Int (Set Beacon) -> Maybe (Set Beacon, Map Int Scanner)
runMerge scanners = mergeScanners (scanners M.! 0)
                                  (M.singleton 0 (V3 0 0 0))
                                  (M.delete 0 scanners)

manhattan :: V3 Int -> V3 Int -> Int
manhattan a b = sum . fmap abs $ (b - a)

day19a :: Solution (Map Int (Set Beacon)) Int
day19a = Solution
  { sParse = parse
  , sShow  = show
  , sSolve = maybeToEither "Unable to merge scanners"
             . fmap (S.size . fst)
             . runMerge
  }

day19b :: Solution (Map Int (Set Beacon)) Int
day19b = Solution
  { sParse = parse
  , sShow  = show
  , sSolve = maybeToEither "Unable to merge scanners"
             . fmap
                 ( maximum
                 . (\sps -> [ manhattan x y | (x : ys) <- tails sps, y <- ys ]
                   )
                 . M.elems
                 . snd
                 )
             . runMerge
  }
