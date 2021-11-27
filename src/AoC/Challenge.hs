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
import           System.Directory
import           System.FilePath
import           System.IO.Error
import qualified Text.Megaparsec               as MP
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
solutionList = []
  {-[ (mkDay_ 1, (Part1, SomeSolution day1a))
  , (mkDay_ 1, (Part2, SomeSolution day1b))
  , (mkDay_ 2, (Part1, SomeSolution day2a))
  , (mkDay_ 2, (Part2, SomeSolution day2b))
  ]-}

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
challengePaths (ChallengeSpec d _) = ChallengePaths
  { _cpInput = "input-data" </> printf "day%02d" d' <.> "txt"
  , _cpTests = "test-data" </> printf "day%02d" d' <.> "txt"
  }
  where d' = dayInt d

makeChallengePathDirs :: ChallengePaths -> IO ()
makeChallengePathDirs ChallengePaths {..} =
  mapM_ (createDirectoryIfMissing True . takeDirectory) [_cpInput, _cpTests]

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe fp = do
  readResult <- tryJust (guard . isDoesNotExistError) (readFile fp)
  traverse (evaluate . force) . eitherToMaybe $ readResult

data ChallengeData = ChallengeData
  { _cdInput :: !(Either [String] String)
  , _cdTests :: ![(String, String)]
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
  return ChallengeData { _cdInput = inp, _cdTests = [] }
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
