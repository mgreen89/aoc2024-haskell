{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day05 (
  day05a,
)
where

-- , day05b

import AoC.Solution
import Data.Bifunctor (first)
import qualified Data.Graph as G
import qualified Data.Graph.Inductive as G
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Void (Void)
import Linear (V2 (..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

parser :: MP.Parsec Void String ([V2 Int], [[Int]])
parser = do
  rules <- MP.sepEndBy ruleParser MP.newline
  MP.newline
  pages <- MP.sepBy pageParser MP.newline
  pure (rules, pages)
 where
  ruleParser :: MP.Parsec Void String (V2 Int)
  ruleParser =
    V2 <$> MPL.decimal <* MP.char '|' <*> MPL.decimal

  pageParser :: MP.Parsec Void String [Int]
  pageParser = MP.sepBy MPL.decimal (MP.char ',')

parse :: String -> Either String ([V2 Int], [[Int]])
parse =
  first MP.errorBundlePretty . MP.parse parser "day05"

applyRules :: G.Gr () () -> [Int] -> [Int]
applyRules g ps =
  G.topsort . G.nfilter (`S.member` pset) $ g
 where
  pset = S.fromList ps

rulesGraph :: [V2 Int] -> G.Gr () ()
rulesGraph rules =
  G.mkUGraph
    (S.toList . S.fromList $ concat [[x, y] | V2 x y <- rules])
    ([(x, y) | V2 x y <- rules])

middle :: [a] -> Maybe a
middle xs = go xs xs
 where
  go (_ : xs') (_ : _ : ys') = go xs' ys'
  go (x : _) _ = Just x
  go [] _ = Nothing

day05a :: Solution ([V2 Int], [[Int]]) Int
day05a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve =
        Right . \(rs, pss) ->
          sum
            [ fromMaybe 0 $ middle test
            | test <- pss
            , test == applyRules (rulesGraph rs) test
            ]
    }

day05b :: Solution _ _
day05b = Solution{sParse = Right, sShow = show, sSolve = Right}
