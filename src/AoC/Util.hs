module AoC.Util
  ( Point
  , cardinalNeighbours
  , allNeighbours
  , parseMap
  , getFreqs
  , strip
  , stripNewlines
  , eitherToMaybe
  , maybeToEither
  , withColor
  ) where

import           Control.Applicative            ( Alternative
                                                , empty
                                                , liftA2
                                                )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Data.Foldable                  ( toList )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Linear.V2                      ( V2(..) )
import           Linear.Vector                  ( basisFor )
import qualified System.Console.ANSI           as ANSI
import           Text.Read                      ( readEither )


--------------------------------------
-- Map helpers (2D, 3D, etc...)
--------------------------------------

type Point = V2 Int

-- | Get all neighbours (including diagonals) of a vector.
allNeighbours :: (Traversable t, Applicative t, Num a) => t a -> [t a]
allNeighbours p =
  -- Take the tail as the first delta is the zero vector.
  tail [ liftA2 (+) p delta | delta <- sequence (pure [0, -1, 1]) ]

-- | Get the cardinal neighbours (i.e. excluding diagonals) of a vector.
cardinalNeighbours :: (Traversable t, Applicative t, Num a) => t a -> [t a]
cardinalNeighbours p =
  [ liftA2 (+) p delta
  | delta <- basisFor p <> fmap (fmap (fmap negate)) basisFor p
  ]

-- | Parse String data into a Map
parseMap :: String -> Either String (Map Point Int)
parseMap = fmap createMap . traverse (traverse (readEither . pure)) . lines
 where
  createMap :: [[Int]] -> Map Point Int
  createMap = M.fromList . concat . zipWith
    (\y -> zipWith (\x -> (V2 x y :: Point, )) [0 ..])
    [0 ..]

--------------------------------------
-- Basic statistics
--------------------------------------

-- | Create a frequency map.
getFreqs :: (Foldable f, Ord a) => f a -> Map a Int
getFreqs = M.fromListWith (+) . fmap (, 1) . toList

--------------------------------------
-- Parsing and string handling
--------------------------------------

-- | Strip trailing and leading whitepsace
strip :: String -> String
strip = T.unpack . T.strip . T.pack

-- | Strip trailing newlines
stripNewlines :: String -> String
stripNewlines = reverse . dropWhile (== '\n') . reverse

--------------------------------------
-- Helpers
--------------------------------------

-- | Convert an 'Either' into a 'Maybe' (or any other 'Alternative' instance),
-- dropping the error value.
eitherToMaybe :: Alternative m => Either e a -> m a
eitherToMaybe = either (const empty) pure

-- | Conver a 'Maybe' into an 'Either' (or any other 'MonadError' instance),
-- using the provided error value if necessary.
maybeToEither :: MonadError e m => e -> Maybe a -> m a
maybeToEither e = maybe (throwError e) pure


--------------------------------------
-- Output
--------------------------------------

-- | Run som IO with the specified foreground colour and intensity.
withColor :: ANSI.ColorIntensity -> ANSI.Color -> IO () -> IO ()
withColor ci c io = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ci c]
  io
  ANSI.setSGR [ANSI.Reset]
