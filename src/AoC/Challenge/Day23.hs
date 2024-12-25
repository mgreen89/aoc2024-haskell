module AoC.Challenge.Day23 (
  day23a,
  day23b,
)
where

import AoC.Common (listTup2)
import AoC.Solution
import AoC.Util (maybeToEither)
import Data.Foldable (maximumBy)
import Data.Functor.Foldable (hylo)
import Data.List (intercalate, isPrefixOf, sort, sortOn)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S

parse :: String -> Either String [(String, String)]
parse =
  maybeToEither "parse error"
    . traverse (listTup2 . splitOn "-")
    . lines

getLinkMap :: (Ord a) => [(a, a)] -> Map a (Set a)
getLinkMap links =
  M.unionsWith
    (<>)
    [ M.fromList [(a, S.singleton b), (b, S.empty)]
    | [a, b] <- sort . (\(a, b) -> [a, b]) <$> links
    ]

solveA :: [(String, String)] -> Int
solveA links =
  length
    . filter (any ("t" `isPrefixOf`))
    $ [ [a, b, c]
      | (a, bs) <- M.toList linkMap
      , b <- S.toList bs
      , c <- S.toList $ linkMap M.! b
      , c `S.member` bs
      ]
 where
  linkMap = getLinkMap links

day23a :: Solution [(String, String)] Int
day23a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . solveA
    }

--  Bron-Kerbosh
{-
bk :: Map String (Set String) -> [Set String]
bk m = go [] S.empty (M.keysSet m) S.empty
 where
  go :: [Set String] -> Set String -> Set String -> Set String -> [Set String]
  go a r p x
    | S.null p && S.null x = a ++ [r]
    | otherwise = go' a p x
   where
    go' :: [Set String] -> Set String -> Set String -> [Set String]
    go' a' p' x' = case S.minView p' of
      Just (v, p'') ->
        go' (go a' (S.insert v r) (S.intersection p' (m M.! v)) (S.intersection x' (m M.! v))) p'' (S.insert v x')
      Nothing -> a'

solveB :: [(String, String)] -> Set String
solveB = last . sortOn S.size . bk . getLinkMap
-}

-- Fix-point of a functor for a labelled list of children.
newtype In a = In {out :: [(String, a)]}
  deriving (Functor)

allCliques :: Map String (Set String) -> [[String]]
allCliques links =
  hylo collapse build (M.toList links)
 where
  build :: [(String, Set String)] -> In [(String, Set String)]
  build =
    In
      . fmap
        ( \(a, ns) ->
            ( a
            , [ (b, ns `S.intersection` (links M.! b))
              | b <- S.toList ns
              ]
            )
        )
  collapse :: In [[String]] -> [[String]]
  collapse =
    foldMap (\(a, bs) -> (a :) <$> if null bs then pure [] else bs) . out

day23b :: Solution [(String, String)] (Set String)
day23b =
  Solution
    { sParse = parse
    , sShow = intercalate "," . S.toAscList
    , sSolve = Right . S.fromList . maximumBy (comparing length) . allCliques . getLinkMap
    }
