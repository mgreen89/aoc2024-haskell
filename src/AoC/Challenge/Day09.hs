module AoC.Challenge.Day09
  ( day09a
  , day09b
  ) where

import           AoC.Solution
import           Control.Monad                  ( mfilter )
import           Data.Foldable                  ( toList )
import           Data.List                      ( sort )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( listToMaybe
                                                , mapMaybe
                                                )
import           Linear.V2                      ( V2(..) )
import           Text.Read                      ( readEither )

type Point = V2 Int

parseMap :: String -> Either String (Map Point Int)
parseMap = fmap createMap . traverse (traverse (readEither . pure)) . lines
 where
  createMap :: [[Int]] -> Map Point Int
  createMap = M.fromList . concat . zipWith
    (\y -> zipWith (\x -> (V2 x y :: Point, )) [0 ..])
    [0 ..]

cardinalNeighbours :: [Point]
cardinalNeighbours = [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

getNeighbours :: Point -> [Point]
getNeighbours p = (+ p) <$> cardinalNeighbours

isLow :: Map Point Int -> (Point, Int) -> Bool
isLow m (p, v) = all isLowerThan (getNeighbours p)
 where
  isLowerThan q = case M.lookup q m of
    Nothing -> True
    Just v' -> v' > v

getLows :: Map Point Int -> [(Point, Int)]
getLows m = filter (isLow m) $ M.toList m

day09a :: Solution (Map Point Int) Int
day09a = Solution { sParse = parseMap
                  , sShow  = show
                  , sSolve = Right . sum . fmap ((+ 1) . snd) . getLows
                  }

-- Map from each point to a lower neighbour, or Nothing if none (or 9, since
-- don't want to count those in part b).
slopeMap :: Map Point Int -> Map Point (Maybe Point)
slopeMap m = M.mapWithKey go m
 where
  go p v = if v > 8
    then Nothing
    else listToMaybe . mapMaybe filt $ getNeighbours p
   where
    -- If the point has a height of less than p, get the point
    -- (and not the height!)
    filt :: Point -> Maybe Point
    filt q = q <$ mfilter (< v) (M.lookup q m)

-- | Create a map of elem -> frequency.
getFreqs :: (Foldable f, Ord a) => f a -> Map a Int
getFreqs = M.fromListWith (+) . map (, 1) . toList

-- Create map of basin lowest-point to basin size.
getBasins :: Map Point Int -> Map Point Int
getBasins m =
  let pointToBasinFloor :: Map Point Point
      pointToBasinFloor = M.mapWithKey go (slopeMap m)
      go :: Point -> Maybe Point -> Point
      go p l = case l of
        Just q  -> pointToBasinFloor M.! q
        Nothing -> p
  in  getFreqs pointToBasinFloor

day09b :: Solution (Map Point Int) Int
day09b = Solution
  { sParse = parseMap
  , sShow  = show
  , sSolve = Right . product . take 3 . reverse . sort . M.elems . getBasins
  }
