{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day23 (
  day23a,
)
where

-- day23a
-- , day23b

import AoC.Common (listTup2)
import AoC.Solution
import AoC.Util (maybeToEither)
import Data.List (isPrefixOf, sort)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

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
  linkMap =
    M.unionsWith
      (<>)
      [ M.fromList [(a, S.singleton b), (b, S.empty)]
      | [a, b] <- sort . (\(a, b) -> [a, b]) <$> links
      ]

day23a :: Solution [(String, String)] Int
day23a =
  Solution
    { sParse =
        maybeToEither "parse error"
          . traverse (listTup2 . splitOn "-")
          . lines
    , sShow = show
    , sSolve = Right . solveA
    }

day23b :: Solution _ _
day23b = Solution{sParse = Right, sShow = show, sSolve = Right}
