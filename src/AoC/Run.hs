module AoC.Run
  ( RunSpec(..)
  , RunOpts(..)
  , defaultRunOpts
  , mainRun
  , SubmitOpts(..)
  , defaultSubmitOpts
  , mainSubmit
  ) where

import           Advent.Extra
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
import           Data.Foldable
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified System.Console.ANSI           as ANSI
import           Text.Printf

-- | Specification for which solutions to run (and optionally test/bench etc...)
data RunSpec
  = RSAll
  | RSDayAll {_rsDay :: Day}
  | RSDayPart {_rsSpec :: ChallengeSpec}
  deriving Show

-- | Options for "run" mode.
data RunOpts = RunOpts
  { _roSpec   :: !RunSpec
  , _roActual :: !Bool
  , _roTest   :: !Bool
  , _roBench  :: !Bool
  }
  deriving Show

-- | Default "run" mode options.
defaultRunOpts :: RunSpec -> RunOpts
defaultRunOpts rs =
  RunOpts { _roSpec = rs, _roActual = True, _roTest = False, _roBench = False }

-- | Options for "submit" mode.
data SubmitOpts = SubmitOpts
  { _soSpec  :: !ChallengeSpec
  , _soTest  :: !Bool
  , _soForce :: !Bool
  }
  deriving Show

-- | Default "submit" mode options.
defaultSubmitOpts :: ChallengeSpec -> SubmitOpts
defaultSubmitOpts cs =
  SubmitOpts { _soSpec = cs, _soTest = True, _soForce = False }

-- | Main function for the "run" mode.
mainRun
  :: (MonadIO m, MonadError [String] m)
  => Config
  -> RunOpts
  -> m (Map Day (Map Part (Either [String] String)))
mainRun cfg ro@RunOpts {..} = do
  solsToRun <- liftEither . first (: []) . filterChallengeMap $ _roSpec
  liftIO $ flip M.traverseWithKey solsToRun $ \d ->
    M.traverseWithKey $ \p -> runOne cfg ro d p

-- | Main function for the "submit" mode.
mainSubmit
  :: (MonadIO m, MonadError [String] m)
  => Config
  -> SubmitOpts
  -> m (Text, SubmitRes)
mainSubmit cfg@Config {..} SubmitOpts {..} = do
  sessKey <- maybeToEither ["ERROR: Session Key required to submit"] _cfgSession
  let ChallengeSpec {..}  = _soSpec
  cd@ChallengeData {..} <- liftIO $ challengeData cfg _soSpec
  dps <- liftEither . first (: []) . getDay challengeMap $ _csDay
  sol                   <- liftEither . first (: []) . getPart dps $ _csPart
  inp                   <- liftEither $ first ("ERROR: No input" :) _cdInput

  when _soTest $ do
    testRes <- liftIO $ runTestSuite sol cd
    unless (and testRes) $ if _soForce
      then do
        liftIO $ withColor ANSI.Vivid ANSI.Red $ putStrLn
          "Forcing submission with test errors!"
      else throwError ["Submission aborted."]
  res <-
    liftEither
    . first (("Solution Error: " :) . (: []) . showSolutionError)
    $ runSomeSolution sol inp
  liftIO $ printf "Submitting solution %s\n" res

  let opts = defaultAoCOpts _cfgYear sessKey
      submit = runAoC opts (AoCSubmit _csDay _csPart res)
  output@(_, status) <- liftEither . first showAoCError =<< liftIO submit

  let (color, out) = displayStatus status
  liftIO $ withColor ANSI.Vivid color $ putStrLn out
  pure output
 where
  displayStatus :: SubmitRes -> (ANSI.Color, String)
  displayStatus sr =
    let color = case sr of
          SubCorrect _     -> ANSI.Green
          SubIncorrect _ _ -> ANSI.Red
          SubWait _        -> ANSI.Yellow
          SubInvalid       -> ANSI.Blue
          SubUnknown _     -> ANSI.Magenta
    in  (color, showAoCSubmitRes sr)

runOne
  :: Config
  -> RunOpts
  -> Day
  -> Part
  -> SomeSolution
  -> IO (Either [String] String)
runOne cfg RunOpts {..} d p sol = do
  withColor ANSI.Dull ANSI.Blue $ printf ">> Day %02d%c" (dayInt d) (partChar p)
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
                evaluate $ force x
                benchmark $ nf sSolve x
                putStrLn "* excluding parsing"
                pure $ Left ["No results when benchmarking"]
              _ -> do
                putStrLn "(No parse)"
                pure $ Left ["No results when benchmarking"]
      | _roActual -> first ((: []) . show) <$> runSol sol inp
      | otherwise -> pure $ Left ["Skipping!"]
    Left e
      | _roTest
      -> pure (Left ["Ran tests and no main input"])
      | otherwise
      -> Left e <$ putStrLn "[INPUT ERROR]" <* traverse_ putStrLn e

runTestSuite :: SomeSolution -> ChallengeData -> IO (Maybe Bool)
runTestSuite sol ChallengeData {..} = do
  res <- traverse (runTestCase sol) _cdTests
  unless (null res) $ do
    let (mark, color) = if and res then ('✓', ANSI.Green) else ('✗', ANSI.Red)
    withColor ANSI.Vivid color
      $ printf "[%c] %d/%d passed\n" mark (length (filter id res)) (length res)
  pure $ and res <$ guard (not (null res))

type SolutionResult = Either SolutionError String

solResString :: SolutionResult -> String
solResString res = case res of
  Right r -> r
  Left  e -> printf "[ERROR] %s" (showSolutionError e)

runTestCase :: SomeSolution -> TestData -> IO Bool
runTestCase sol TestData {..} = do
  withColor ANSI.Dull color $ printf "[%c]" mark
  printf " (%s)" (solResString res)
  if pass
    then printf "\n"
    else withColor ANSI.Vivid ANSI.Red $ printf " (Expected: %s)\n" _tdAnswer
  return pass
 where
  res                 = runSomeSolution sol _tdInput
  (mark, pass, color) = case res of
    Right r -> if strip _tdAnswer == strip r
      then ('✓', True, ANSI.Green)
      else ('✗', False, ANSI.Red)
    Left _ -> ('✗', False, ANSI.Red)

runSol :: SomeSolution -> String -> IO SolutionResult
runSol sol inp = do
  printf " %s\n" (solResString res)
  return res
  where res = runSomeSolution sol inp

filterChallengeMap :: RunSpec -> Either String ChallengeMap
filterChallengeMap = \case
  RSAll      -> pure challengeMap
  RSDayAll d -> do
    ps <- getDay challengeMap d
    pure $ M.singleton d ps
  RSDayPart (ChallengeSpec d p) -> do
    ps <- getDay challengeMap d
    c  <- getPart ps p
    pure $ M.singleton d (M.singleton p c)
