{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Common.Point (
  cardinalDiffs,
  allDiffs,
  cardinalNeighbs,
  allNeighbs,
  manhattan,
  boundingBox,
  boundingBox',
  inBoundingBox,
  parse2dMap,
  parse2dCharMap,
  parse2dReadMap,
  Dir (..),
  dirFromArrow,
  dirRot,
  dirPoint,
) where

import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Semigroup
import Data.Semigroup.Foldable
import GHC.Generics (Generic)
import Linear (Additive, V2 (..), basis, negated)
import Text.Read (readEither)

-- Cardinal neighbour moves.
cardinalDiffs :: (Traversable t, Additive t, Num a) => [t a]
cardinalDiffs = fmap negated basis <> basis

-- | Get the cardinal neighbours (i.e. excluding diagonals) of a vector.
cardinalNeighbs ::
  (Traversable t, Additive t, Applicative t, Num a) =>
  t a ->
  [t a]
cardinalNeighbs p =
  [liftA2 (+) p delta | delta <- cardinalDiffs]

-- All neighbour moves.
allDiffs :: (Traversable t, Applicative t, Num a) => [t a]
allDiffs =
  -- Take the tail as the first delta is the zero vector.
  tail $ sequence (pure [0, -1, 1])

-- | Get all neighbours (including diagonals) of a vector.
allNeighbs :: (Traversable t, Applicative t, Num a) => t a -> [t a]
allNeighbs p =
  [liftA2 (+) p delta | delta <- allDiffs]

-- | Get the manhattan distance between two vectors.
manhattan :: (Foldable f, Num a, Num (f a)) => f a -> f a -> a
manhattan x y = sum . abs $ x - y

-- | Get the bounding box of a collection of points.
boundingBox :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> (g a, g a)
boundingBox =
  (\(Ap mi, Ap ma) -> (getMin <$> mi, getMax <$> ma))
    . foldMap1 (\p -> (Ap (Min <$> p), Ap (Max <$> p)))

-- | `boundingBox` that safely works on generic (possible empty) foldables.
boundingBox' :: (Foldable f, Applicative g, Ord a) => f (g a) -> Maybe (g a, g a)
boundingBox' = fmap boundingBox . NE.nonEmpty . toList

-- | Check if a point is in a bounding box.
inBoundingBox :: (Applicative g, Foldable g, Ord a) => (g a, g a) -> g a -> Bool
inBoundingBox (bMin, bMax) p = and $ go <$> p <*> bMin <*> bMax
 where
  go cp cmin cmax = cp >= cmin && cp <= cmax

-- | Parse data into a 2D Map
parse2dMap :: [[a]] -> Map (V2 Int) a
parse2dMap =
  M.fromList
    . concat
    . zipWith (\y -> zipWith (\x -> (V2 x y,)) [0 ..]) [0 ..]

-- | Parse String data into a 2D Map
parse2dCharMap :: String -> Map (V2 Int) Char
parse2dCharMap =
  parse2dMap . lines

-- | Parse `Read`able into a 2D Map
parse2dReadMap :: (Read a) => String -> Either String (Map (V2 Int) a)
parse2dReadMap =
  fmap parse2dMap
    . traverse (traverse (readEither . pure))
    . lines

{- | Direction
Up, Right, Left and Down.
-}
data Dir = U | R | D | L deriving (Show, Eq, Ord, Enum, Generic, NFData)

-- | Parse a direction from an "arrow" character
dirFromArrow :: Char -> Either String Dir
dirFromArrow = \case
  '^' -> Right U
  '>' -> Right R
  'v' -> Right D
  '<' -> Right L
  x -> Left ("Invalid direction arrow: " ++ [x])

-- | Rotate a direction.
dirRot :: Dir -> Dir -> Dir
dirRot U = id
dirRot R = \case
  U -> R
  R -> D
  D -> L
  L -> U
dirRot D = \case
  U -> D
  R -> L
  D -> U
  L -> R
dirRot L = \case
  U -> L
  R -> U
  D -> R
  L -> D

{- | Convert a direction to a unit vector in the 2D plane.
N.B. that x increases to the right, y increases going down.
-}
dirPoint :: Dir -> V2 Int
dirPoint = \case
  U -> V2 0 (-1)
  R -> V2 1 0
  D -> V2 0 1
  L -> V2 (-1) 0

instance Semigroup Dir where
  (<>) :: Dir -> Dir -> Dir
  (<>) = dirRot

  stimes :: (Integral b) => b -> Dir -> Dir
  stimes n d = case n `mod` 4 of
    1 -> d
    2 -> d <> d
    3 -> d <> D
    _ -> U

instance Monoid Dir where
  mempty :: Dir
  mempty = U
