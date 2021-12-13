module AoC.Challenge.Day13
  ( day13a
  , day13b
  ) where

import           AoC.Solution
import           AoC.Util                       ( Point )
import           Data.Foldable                  ( foldl' )
import           Data.List.Split                ( splitOn )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Linear.V2                      ( V2(..) )
import           Text.Read                      ( readEither )

type Dot = Point
type Fold = Point

listTo2Tuple :: [a] -> Either String (a, a)
listTo2Tuple [a, b] = Right (a, b)
listTo2Tuple _      = Left "Not a 2-elem list"

parse :: String -> Either String ([Dot], [Fold])
parse inp = do
  (dotsInp, foldsInp) <- listTo2Tuple . splitOn "\n\n" $ inp
  dots                <- traverse parseDot . lines $ dotsInp
  folds               <- traverse parseFold . lines $ foldsInp
  pure (dots, folds)
 where
  parseDot :: String -> Either String Dot
  parseDot l = do
    (a, b) <- listTo2Tuple . splitOn "," $ l
    x      <- readEither a
    y      <- readEither b
    pure $ V2 x y
  parseFold :: String -> Either String Fold
  parseFold l = do
    (intro, vStr) <- listTo2Tuple . splitOn "=" $ l
    v             <- readEither vStr
    let ax = last intro
    case ax of
      'x' -> pure $ V2 v 0
      'y' -> pure $ V2 0 v
      _   -> Left $ "Invalid axis: " <> pure ax

-- Apply a fold to a single dot.
applyFold :: Fold -> Dot -> Dot
applyFold f d@(V2 dx dy) = case f of
  V2 x 0 | dx > x    -> V2 (2 * x - dx) dy
         | otherwise -> d
  V2 0 y | dy > y    -> V2 dx (2 * y - dy)
         | otherwise -> d
  _ -> error $ "Invalid fold: " <> show f

runFold :: Fold -> Set Dot -> Set Dot
runFold f = S.map (applyFold f)

day13a :: Solution ([Dot], [Fold]) Int
day13a = Solution
  { sParse = parse
  , sShow  = show
  , sSolve = \(ds, fs) -> Right . S.size . runFold (head fs) . S.fromList $ ds
  }

showDots :: Set Point -> String
showDots s =
  let xMax = maximum $ (\(V2 x _) -> x) <$> S.elems s
      yMax = maximum $ (\(V2 _ y) -> y) <$> S.elems s
  in  foldMap
        (\y ->
          foldMap (\x -> if S.member (V2 x y) s then "#" else ".") [0 .. xMax]
            <> "\n"
        )
        [0 .. yMax]

day13b :: Solution ([Dot], [Fold]) (Set Dot)
day13b = Solution
  { sParse = parse
  , sShow  = \s -> "\n" <> showDots s
  , sSolve = Right . \(ds, fs) -> foldl' (flip runFold) (S.fromList ds) fs
  }
