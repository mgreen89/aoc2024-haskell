module AoC.Challenge.Day22
  ( day22a
  , day22b
  ) where

import           AoC.Solution
import           Control.Applicative            ( liftA2 )
import           Control.DeepSeq                ( NFData )
import           Data.Bifunctor                 ( first )
import           Data.Foldable                  ( asum
                                                , foldl'
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Void                      ( Void )
import           GHC.Generics                   ( Generic )
import           Linear.V3                      ( V3(..) )
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MP
import qualified Text.Megaparsec.Char.Lexer    as MPL

type Cuboid = (V3 Int, V3 Int)

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

-- Find the intersection of two cuboids.
intersect :: Cuboid -> Cuboid -> Maybe Cuboid
intersect (aMin, aMax) (bMin, bMax) =
  let (iMin, iMax) = (max <$> aMin <*> bMin, min <$> aMax <*> bMax)
  in  if and (liftA2 (<=) iMin iMax) then Just (iMin, iMax) else Nothing

-- Find the size of a cuboid.
cuboidSize :: Cuboid -> Int
cuboidSize (cMin, cMax) = product . fmap ((+ 1) . abs) $ cMax - cMin

-- Follow the instructions and return the number of cubes turned on
-- at the end of the instructions.
process :: [Instruction] -> Int
process = snd . foldl' go ([], 0)
 where
  go :: ([(Cuboid, Int)], Int) -> Instruction -> ([(Cuboid, Int)], Int)
  go (cs, n) Instruction {..} =
    let iCube = (iMin, iMax)
        intersections =
          mapMaybe (\(c, i) -> (, negate i) <$> intersect iCube c) cs
        addCube = [ (iCube, 1) | iOn ]
        delta   = countNodes intersections + countNodes addCube
    in  (addCube <> intersections <> cs, n + delta)

  countNodes :: [(Cuboid, Int)] -> Int
  countNodes = sum . fmap (\(c, i) -> i * cuboidSize c)

partAFilter :: Instruction -> Bool
partAFilter Instruction {..} = all (>= -50) iMax && all (<= 50) iMin

day22a :: Solution [Instruction] Int
day22a = Solution { sParse = parse
                  , sShow  = show
                  , sSolve = Right . process . filter partAFilter
                  }

day22b :: Solution [Instruction] Int
day22b = Solution { sParse = parse, sShow = show, sSolve = Right . process }
