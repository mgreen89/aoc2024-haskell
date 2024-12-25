{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day24 (
  day24a,
)
where

-- , day24b

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Bits (shiftL)
import Data.Foldable (foldl')
import Data.Function (fix)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

type Wire = String

data Gate = And Wire Wire Wire | Or Wire Wire Wire | Xor Wire Wire Wire
  deriving (Show, Eq, Ord, Generic, NFData)

parser :: MP.Parsec Void String ([(Wire, Bool)], [Gate])
parser = do
  (,) <$> (inits <* MP.newline) <*> gates
 where
  -- e.g. x00: 1
  inits = flip MP.sepEndBy MP.newline $ do
    wname <- MP.label "w" (MP.count 3 MP.alphaNumChar)
    MP.string ": "
    val <- MPL.decimal
    pure (wname, val == 1)

  -- e.g. x00 AND y00 -> z00
  gates = flip MP.sepBy MP.newline $ do
    x <- MP.label "x" (MP.count 3 MP.alphaNumChar) <* MP.hspace
    gate <-
      MP.choice
        [ And <$ MP.string "AND"
        , Or <$ MP.string "OR"
        , Xor <$ MP.string "XOR"
        ]
    y <- MP.hspace *> MP.count 3 MP.alphaNumChar
    MP.string " -> "
    o <- MP.count 3 MP.alphaNumChar
    pure $ gate x y o

parse :: String -> Either String ([(Wire, Bool)], [Gate])
parse =
  first MP.errorBundlePretty . MP.parse parser "day24"

fixa :: (Eq t) => (t -> t) -> t -> t
fixa iter z =
  let z' = iter z
   in if z' == z then z else fixa iter z'

solveA :: ([(Wire, Bool)], [Gate]) -> Int
solveA (inits, gates) =
  sum
    . fmap ((\(k, a) -> a `shiftL` k) . (\(k, a) -> (read $ drop 1 k, if a then 1 else 0)))
    . M.toList
    . M.filterWithKey (\k _ -> head k == 'z')
    $ fixa step (M.fromList inits)
 where
  step :: Map Wire Bool -> Map Wire Bool
  step ws = foldl' go ws gates

  go :: Map Wire Bool -> Gate -> Map Wire Bool
  go ws g =
    let
      (x, y, z, op) =
        ( case g of
            (And a b c) -> (a, b, c, (&&))
            (Or a b c) -> (a, b, c, (||))
            (Xor a b c) -> (a, b, c, (/=))
        )
     in
      case op <$> (ws M.!? x) <*> (ws M.!? y) of
        Just o -> M.insert z o ws
        Nothing -> ws

day24a :: Solution ([(Wire, Bool)], [Gate]) Int
day24a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}

{-
 For a ripple adder:

 c0 x0 y0 -> c1 z0

 c0 0:
     x00     1
 y00 (0,0) (0,1)
   1 (0,1) (1,0)

 c0 1:
     x00     1
 y00 (0,1) (1,0)
   1 (1,0) (1,1)

Looks like this is always done by:

  xn ^ yn -> a
  xn & yn -> b

  a ^ c(n-1) -> zn
  a & c(n-1) -> d
  d | b -> cn
-}

day24b :: Solution _ _
day24b = Solution{sParse = Right, sShow = show, sSolve = Right}
