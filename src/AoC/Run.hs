module AoC.Run
  ( TestSpec(..)
  , RunOpts(..)
  , defaultRunOpts
  , mainRun
  , SubmitOpts(..)
  , defaultSubmitOpts
  , mainSubmit
  ) where

import           Advent
import           AoC.Challenge
import           AoC.Config
import           AoC.Solution
import           AoC.Util
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Criterion
import           Data.Bifunctor
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified System.Console.ANSI           as ANSI
import           Text.Printf

data TestSpec
  = TSAll
  | TSDayAll {_tsDay :: Day}
  | TSDayPart {_tsSpec :: ChallengeSpec}
  deriving Show

data RunOpts = RunOpts
  { _roSpec   :: !TestSpec
  , _roActual :: !Bool
  , _roTest   :: !Bool
  , _roBench  :: !Bool
  }
  deriving Show

data SubmitOpts = SubmitOpts
  { _soSpec  :: !ChallengeSpec
  , _soTest  :: !Bool
  , _soForce :: !Bool
  }
  deriving Show

defaultRunOpts :: TestSpec -> RunOpts
defaultRunOpts ts =
  RunOpts { _roSpec = ts, _roActual = True, _roTest = False, _roBench = False }

defaultSubmitOpts :: ChallengeSpec -> SubmitOpts
defaultSubmitOpts cs =
  SubmitOpts { _soSpec = cs, _soTest = True, _soForce = False }

mainRun
  :: (MonadIO m, MonadError [String] m)
  => Config
  -> RunOpts
  -> m (Map Day (Map Part (Either [String] String)))
mainRun cfg ro@RunOpts {..} = do
  solsToRun <- liftEither . first (: []) . filterChallengeMap $ _roSpec
  liftIO $ runAll cfg ro solsToRun

mainSubmit
  :: (MonadIO m, MonadError [String] m)
  => Config
  -> SubmitOpts
  -> m (Text, SubmitRes)
mainSubmit cfg SubmitOpts {..} = do
  liftIO $ withColor ANSI.Dull ANSI.Red $ putStrLn "Submit mode not implemented"
  pure (mempty, SubUnknown "Not Implemented")

runAll
  :: Config
  -> RunOpts
  -> ChallengeMap
  -> IO (Map Day (Map Part (Either [String] String)))
runAll cfg ro cm =
  flip M.traverseWithKey cm $ \d -> M.traverseWithKey $ \p -> runOne cfg ro d p

runOne
  :: Config
  -> RunOpts
  -> Day
  -> Part
  -> SomeSolution
  -> IO (Either [String] String)
runOne cfg ro@RunOpts {..} d p sol = do
  let ChallengePaths {..} = challengePaths (ChallengeSpec d p)
  withColor ANSI.Dull ANSI.Blue
    $ printf ">> Day %02d%c" (dayInt d) (partChar p)
  cd@ChallengeData {..} <- challengeData cfg (ChallengeSpec d p)
  if _roTest
    then do
      printf "\n"
      runTestSuite sol cd
      printf "Answer :"
    else printf " "
  case _cdInput of
    Right inp
      | _roBench -> do
        _ <- evaluate (force inp)
        case sol of
          SomeSolution Solution {..} -> do
            let i = sParse inp
            case i of
              Right x -> do
                _ <- evaluate (force x)
                benchmark (nf sSolve x)
                putStrLn "* excluding parsing"
                pure $ Left ["No results when benchmarking"]
              _ -> do
                putStrLn "(No parse)"
                pure $ Left ["No results when benchmarking"]
      | _roActual -> first ((: []) . show) <$> runSol sol inp
      | otherwise -> pure $ Left ["Skipping!"]
    Left e
      | _roTest -> pure (Left ["Ran tests and no main input"])
      | otherwise -> Left e <$ putStrLn "[INPUT ERROR]" <* mapM_ putStrLn e

runTestSuite :: SomeSolution -> ChallengeData -> IO (Maybe Bool)
runTestSuite sol ChallengeData {..} = do
  res <- traverse (runTestCase sol) _cdTests
  unless (null res) $ do
    let (mark, color) = if and res then ('✓', ANSI.Green) else ('✗', ANSI.Red  )
    withColor ANSI.Vivid color $
      printf "[%c] %d/%d passed\n" mark (length (filter id res)) (length res)
  pure $ and res <$ guard (not (null res))

type SolutionResult = Either SolutionError String

solResString :: SolutionResult -> String
solResString res =
  case res of
    Right r           -> r
    Left  (SEParse e) -> printf "ERROR Parse: %s" e
    Left  (SESolve e) -> printf "ERROR Solve: %s" e

runTestCase :: SomeSolution -> TestData -> IO Bool
runTestCase sol TestData{..} = do
  withColor ANSI.Dull color $ printf "[%c]" mark
  printf " (%s)" (solResString res)
  if pass
    then printf "\n"
    else withColor ANSI.Vivid ANSI.Red $ printf " (Expected: %s)\n" _tdAnswer
  return pass
 where
  res    = runSomeSolution sol _tdInput
  (mark, pass, color) = case res of
      Right r -> if strip _tdAnswer == strip r
        then ('✓', True, ANSI.Green)
        else ('✗', False, ANSI.Red)
      Left _ -> ('✗', False, ANSI.Red)

runSol :: SomeSolution -> String -> IO SolutionResult
runSol sol inp = do
  printf " %s\n" (solResString res)
  return res
 where
  res    = runSomeSolution sol inp

runSolution :: Solution a b -> String -> Either SolutionError String
runSolution Solution {..} inp = do
  x <- first SEParse . sParse $ stripNewlines inp
  y <- first SESolve . sSolve $ x
  pure $ sShow y

runSomeSolution :: SomeSolution -> String -> Either SolutionError String
runSomeSolution (SomeSolution s) = runSolution s

filterChallengeMap :: TestSpec -> Either String ChallengeMap
filterChallengeMap = \case
  TSAll      -> pure challengeMap
  TSDayAll d -> do
    ps <- getDay d
    pure $ M.singleton d ps
  TSDayPart (ChallengeSpec d p) -> do
    ps <- getDay d
    c  <- maybeToEither (printf "Part not found: %c" (partChar p))
      $ M.lookup p ps
    pure $ M.singleton d (M.singleton p c)
 where
  getDay :: Day -> Either String (Map Part SomeSolution)
  getDay d =
    maybeToEither (printf "Day not yet available: %d" (dayInt d))
      $ M.lookup d challengeMap
