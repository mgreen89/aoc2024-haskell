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
import Data.List (intercalate, isPrefixOf, sort)
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

-- Fix-point of a functor for a labelled list of children.
newtype In a = In {out :: [(String, a)]}
  deriving (Functor)

allCliques :: Map String (Set String) -> [[String]]
allCliques links =
  hylo collapse build (M.toList links)
 where
  -- Build a recursive data-structure where every element has a label and
  -- contains the set of all possible children.
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

  -- Collapse the recursive data-structure into a list of labels for
  -- each nested element.
  collapse :: In [[String]] -> [[String]]
  collapse =
    foldMap (\(a, bs) -> (a :) <$> if null bs then pure [] else bs) . out

day23b :: Solution [(String, String)] [String]
day23b =
  Solution
    { sParse = parse
    , sShow = intercalate ","
    , sSolve = Right . maximumBy (comparing length) . allCliques . getLinkMap
    }
