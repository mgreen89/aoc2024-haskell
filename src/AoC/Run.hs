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
import           Data.Foldable
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
  liftIO $ flip M.traverseWithKey solsToRun $ \d ->
    M.traverseWithKey $ \p -> runOne cfg ro d p

mainSubmit
  :: (MonadIO m, MonadError [String] m)
  => Config
  -> SubmitOpts
  -> m (Text, SubmitRes)
mainSubmit cfg@Config {..} SubmitOpts {..} = do
  let ChallengeSpec {..}  = _soSpec
      ChallengePaths {..} = challengePaths _soSpec
  cd@ChallengeData {..} <- liftIO $ challengeData cfg _soSpec
  dps <- liftEither . first (: []) . getDay challengeMap $ _csDay
  sol                   <- liftEither . first (: []) . getPart dps $ _csPart
  inp                   <- liftEither $ first ("ERROR: No input" :) _cdInput
  opts                  <-
    defaultAoCOpts _cfgYear
      <$> maybeToEither ["ERROR: Session Key required to submit"] _cfgSession

  when _soTest $ do
    testRes <- liftIO $ runTestSuite sol cd
    unless (and testRes) $ if _soForce
      then do
        liftIO $ withColor ANSI.Vivid ANSI.Red $ putStrLn
          "Forcing submission with test errors!"
      else throwError ["Submission aborted."]
  res <-
    liftEither
    . first (("Solution Error: " :) . (: []) . show)
    $ runSomeSolution sol inp
  liftIO $ printf "Submitting solution %s\n" res

  let submit = runAoC opts (AoCSubmit _csDay _csPart res)
  output@(resp, status) <- liftEither . first showAoCError =<< liftIO submit

  let (color, out) = displayStatus status
  liftIO $ withColor ANSI.Vivid color $ putStrLn out
  pure output

displayStatus :: SubmitRes -> (ANSI.Color, String)
displayStatus = \case
  SubCorrect r     -> (ANSI.Green, correctMsg r)
  SubIncorrect t h -> (ANSI.Red, incorrectMsg t h)
  SubWait t ->
    let (m, s) = t `divMod` 60
        resp =
          printf "Answer re-submitted too soon.  Please wait %dmin %dsec" m s
    in  (ANSI.Yellow, resp)
  SubInvalid{} ->
    ( ANSI.Blue
    , "Submission was rejected.  Maybe not unlocked yet, or already answered?"
    )
  SubUnknown{} -> (ANSI.Magenta, "Response from server was not recognized.")
 where
  correctMsg Nothing  = "Answer was correct!"
  correctMsg (Just r) = printf
    "Answer was correct, and you made the global leaderboard at rank %d !!"
    r
  incorrectMsg t h = printf
    "Answer was incorrect!%s  Please wait %d before submitting again"
    hintStr
    (t `div` 60)
   where
    hintStr :: String
    hintStr = case h of
      Nothing -> ""
      Just s  -> printf "  Hint: Answer was %s." s

runOne
  :: Config
  -> RunOpts
  -> Day
  -> Part
  -> SomeSolution
  -> IO (Either [String] String)
runOne cfg RunOpts {..} d p sol = do
  let ChallengePaths {..} = challengePaths (ChallengeSpec d p)
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
        evaluate (force inp)
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
  Right r           -> r
  Left  (SEParse e) -> printf "ERROR Parse: %s" e
  Left  (SESolve e) -> printf "ERROR Solve: %s" e

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

runSolution :: Solution a b -> String -> Either SolutionError String
runSolution Solution {..} inp = do
  x <- first SEParse . sParse $ stripNewlines inp
  y <- first SESolve . sSolve $ x
  pure $ sShow y

runSomeSolution :: SomeSolution -> String -> Either SolutionError String
runSomeSolution (SomeSolution s) = runSolution s

getDay :: ChallengeMap -> Day -> Either String (Map Part SomeSolution)
getDay cm d =
  maybeToEither (printf "Day not yet available: %d" (dayInt d))
    $ M.lookup d challengeMap

getPart :: Map Part SomeSolution -> Part -> Either String SomeSolution
getPart ps p =
  maybeToEither (printf "Part not found: %c" (partChar p)) $ M.lookup p ps

filterChallengeMap :: TestSpec -> Either String ChallengeMap
filterChallengeMap = \case
  TSAll      -> pure challengeMap
  TSDayAll d -> do
    ps <- getDay challengeMap d
    pure $ M.singleton d ps
  TSDayPart (ChallengeSpec d p) -> do
    ps <- getDay challengeMap d
    c  <- getPart ps p
    pure $ M.singleton d (M.singleton p c)
