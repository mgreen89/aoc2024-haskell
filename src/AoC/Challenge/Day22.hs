{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module AoC.Challenge.Day22
  ( day22a
  -- , day22b
  ) where

import           AoC.Solution
import           Control.DeepSeq                ( NFData )
import           Data.Bifunctor                 ( first )
import           Data.Foldable                  ( asum, foldl' )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Void                      ( Void )
import           GHC.Generics                   ( Generic )
import           Linear.V3                      ( V3(..) )
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MP
import qualified Text.Megaparsec.Char.Lexer    as MPL

data Instruction = Instruction
  { iOn  :: Bool
  , iMin :: V3 Int
  , iMax :: V3 Int
  }
  deriving (Show, Generic, NFData)

instrParser :: MP.Parsec Void String Instruction
instrParser = do
  iOn <- asum [True <$ MP.string "on", False <$ MP.string "off"] <* MP.space
  (xMin, xMax) <- parseRange "x" <* MP.char ','
  (yMin, yMax) <- parseRange "y" <* MP.char ','
  (zMin, zMax) <- parseRange "z"
  pure $ Instruction { iOn  = iOn
                     , iMin = V3 xMin yMin zMin
                     , iMax = V3 xMax yMax zMax
                     }
 where
  parseRange ax = do
    MP.string ax
    MP.char '='
    lb <- MPL.signed MP.space MPL.decimal
    MP.string ".."
    ub <- MPL.signed MP.space MPL.decimal
    pure (lb, ub)

parse :: String -> Either String [Instruction]
parse =
  first MP.errorBundlePretty . traverse (MP.parse instrParser "day22") . lines

partA :: [Instruction] -> Int
partA = S.size . foldl' go S.empty . filter f
 where
  f :: Instruction -> Bool
  f Instruction {..} = all (>= -50) iMax || all (<= 50) iMin

  toCoords :: Instruction -> [V3 Int]
  toCoords Instruction {..} =
    let (V3 xMin yMin zMin) = iMin
        (V3 xMax yMax zMax) = iMax
    in  [ V3 x y z
        | x <- [(max xMin (-50)) .. (min xMax 50)]
        , y <- [(max yMin (-50)) .. (min yMax 50)]
        , z <- [(max zMin (-50)) .. (min zMax 50)]
        ]

  go :: Set (V3 Int) -> Instruction -> Set (V3 Int)
  go s i =
    let iSet = S.fromList . toCoords $ i
    in  if iOn i then S.union s iSet else S.difference s iSet


day22a :: Solution [Instruction] Int
day22a = Solution { sParse = parse, sShow = show, sSolve = Right . partA }

day22b :: Solution _ _
day22b = Solution { sParse = Right, sShow = show, sSolve = Right }
