module AoC.Challenge
  ( ChallengeSpec(..)
  , SomeSolution(..)
  , SolutionError(..)
  , ChallengeMap(..)
  , challengeMap
  , ChallengePaths(..)
  , challengePaths
  , ChallengeData(..)
  , challengeData
  , TestData(..)
  ) where

import           AoC.Challenge.Day01           as AoC
import           AoC.Challenge.Day02           as AoC
import           AoC.Challenge.Day03           as AoC
import           AoC.Challenge.Day04           as AoC
import           AoC.Challenge.Day05           as AoC
import           AoC.Challenge.Day06           as AoC
import           AoC.Challenge.Day07           as AoC
import           AoC.Challenge.Day08           as AoC
import           AoC.Challenge.Day09           as AoC
import           AoC.Challenge.Day10           as AoC
import           AoC.Challenge.Day11           as AoC
import           AoC.Challenge.Day12           as AoC
import           AoC.Challenge.Day13           as AoC
import           AoC.Challenge.Day14           as AoC
import           AoC.Challenge.Day15           as AoC
import           AoC.Challenge.Day16           as AoC
import           AoC.Challenge.Day17           as AoC
import           AoC.Challenge.Day18           as AoC
import           AoC.Challenge.Day19           as AoC
import           AoC.Challenge.Day20           as AoC
import           AoC.Challenge.Day21           as AoC
import           AoC.Challenge.Day22           as AoC
import           AoC.Challenge.Day23           as AoC
import           AoC.Challenge.Day24           as AoC
import           AoC.Challenge.Day25           as AoC

import           Advent
import           AoC.Config
import           AoC.Solution
import           AoC.Util
import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Foldable
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import           System.Directory
import           System.FilePath
import           System.IO.Error
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MP
import           Text.Printf

data ChallengeSpec = ChallengeSpec
  { _csDay  :: Day
  , _csPart :: Part
  }
  deriving Show

data SolutionError
  = SEParse String
  | SESolve String
  deriving (Show)

data SomeSolution where
  -- NFData for two reasons:
  --  `a` so that the parse function can be forced, before...
  --  `b` so that the solve function can be benchmarked.
  SomeSolution ::(NFData a, NFData b) => Solution a b -> SomeSolution

type ChallengeMap = Map Day (Map Part SomeSolution)

challengeMap :: ChallengeMap
challengeMap =
  M.unionsWith M.union
    . map (uncurry M.singleton . second (uncurry M.singleton))
    $ solutionList

solutionList :: [(Day, (Part, SomeSolution))]
solutionList =
  [ (mkDay_ 1, (Part1, SomeSolution day01a))
  , (mkDay_ 1, (Part2, SomeSolution day01b))
  {-, (mkDay_ 2, (Part1, SomeSolution day2a))
  , (mkDay_ 2, (Part2, SomeSolution day2b))
  -}
  ]

showAoCError :: AoCError -> [String]
showAoCError = \case
  AoCClientError e ->
    [ "Error contacting Advent of Code server to fetch input"
    , "Possible invalid session key"
    , printf "Server response: %s" (show e)
    ]
  AoCReleaseError t ->
    ["Challenge not yet released!", printf "Please wait %s" (show t)]
  AoCThrottleError -> ["Too many requests at a time.  Please slow down."]

data ChallengePaths = ChallengePaths
  { _cpInput :: !FilePath
  , _cpTests :: !FilePath
  }
  deriving Show

challengePaths :: ChallengeSpec -> ChallengePaths
challengePaths (ChallengeSpec d p) = ChallengePaths
  { _cpInput = "data" </> "input" </> printf "day%02d" d' <.> "txt"
  , _cpTests = "data" </> "test" </> printf "day%02d%c" d' p' <.> "txt"
  }
  where
    d' = dayInt d
    p' = partChar p

makeChallengePathDirs :: ChallengePaths -> IO ()
makeChallengePathDirs ChallengePaths {..} =
  mapM_ (createDirectoryIfMissing True . takeDirectory) [_cpInput, _cpTests]

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe fp = do
  readResult <- tryJust (guard . isDoesNotExistError) (readFile fp)
  traverse (evaluate . force) . eitherToMaybe $ readResult

data ChallengeData = ChallengeData
  { _cdInput :: !(Either [String] String)
  , _cdTests :: ![TestData]
  }

challengeData :: Config -> ChallengeSpec -> IO ChallengeData
challengeData Config {..} spec@ChallengeSpec {..} = do
  makeChallengePathDirs cps
  inp <-
    runExceptT
    . asum
    $ [ maybeToEither [printf "Input file not found at %s" _cpInput]
        =<< liftIO (readFileMaybe _cpInput)
      , fetchInput
      ]
  ts <- readFileMaybe _cpTests >>= \case
    Nothing -> pure []
    Just s  -> case MP.parse parseTests _cpTests s of
      -- Put [] in the IO functor (no test data), and print an error.
      Left  e -> [] <$ putStrLn (MP.errorBundlePretty e)
      Right r -> pure r

  return ChallengeData { _cdInput = inp, _cdTests = ts }
 where
  cps@ChallengePaths {..} = challengePaths spec
  fetchInput :: ExceptT [String] IO String
  fetchInput = do
    s <- maybeToEither ["Session key needed to fetch input"] _cfgSession
    let opts = defaultAoCOpts _cfgYear s
    inp <- liftEither . bimap showAoCError T.unpack =<< liftIO (runAoC opts a)
    liftIO $ writeFile _cpInput inp
    pure inp
    where a = AoCInput _csDay


data TestData = TestData
  { _tdInput  :: String
  , _tdAnswer :: String
  }

parseTests :: MP.Parsec Void String [TestData]
parseTests = MP.many parseTest <* MP.eof

parseTest :: MP.Parsec Void String TestData
parseTest = do
  inp <- MP.manyTill MP.anySingle $ MP.lookAhead (MP.string ">>>")
  ans <-
    MP.string ">>>" *> MP.space1 *> MP.many (MP.anySingleBut '\n') <* MP.single
      '\n'
  pure TestData { _tdInput = inp, _tdAnswer = ans }
