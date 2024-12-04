module AoC.Challenge.Day03 (
  day03a,
  day03b,
)
where

import AoC.Solution
import Data.Bifunctor (first)
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

dropUntil :: MP.Parsec Void String a -> MP.Parsec Void String a
dropUntil = MP.try . MP.skipManyTill MP.anySingle . MP.try

mul :: MP.Parsec Void String Int
mul = do
  MP.string "mul("
  a <- MPL.decimal
  MP.char ','
  b <- MPL.decimal
  MP.char ')'
  pure $ a * b

parserA :: MP.Parsec Void String Int
parserA = sum <$> MP.some (dropUntil mul)

parseA :: String -> Either String Int
parseA =
  first MP.errorBundlePretty . MP.parse parserA "day03a"

day03a :: Solution Int Int
day03a = Solution{sParse = parseA, sShow = show, sSolve = Right}

parserB :: MP.Parsec Void String Int
parserB =
  sum <$> enabled
 where
  enabled :: MP.Parsec Void String [Int]
  enabled =
      MP.option [] . dropUntil $ MP.choice
        [ MP.string "don't()" *> disabled
        , (:) <$> mul <*> enabled
        ]

  disabled :: MP.Parsec Void String [Int]
  disabled = MP.option [] $ dropUntil (MP.string "do()") *> enabled

parseB :: String -> Either String Int
parseB =
  first MP.errorBundlePretty . MP.parse parserB "day03b"

day03b :: Solution Int Int
day03b = Solution{sParse = parseB, sShow = show, sSolve = Right}
