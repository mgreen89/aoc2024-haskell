{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

import           Advent
import           AoC
import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens            hiding ( argument )
import           Control.Monad
import           Control.Monad.Except
import           Data.Char
import           Data.Foldable
import           Data.IORef
import           Data.List                      ( intercalate )
import qualified Data.Map                      as M
import           Data.Maybe
import           Options.Applicative
import qualified System.Console.ANSI           as ANSI
import           Text.Printf
import           Text.Read

data Mode
  = Run RunOpts
  | Submit SubmitOpts

data Opts = O
  { _oMode   :: !Mode
  , _oConfig :: !(Maybe FilePath)
  }

main :: IO ()
main = do
  inputCache <- newIORef Nothing
  O {..}     <- execParser $ info
    (parseOpts inputCache <**> helper)
    (  fullDesc
    <> header "aoc-dev - Advent of Code interactive dev env"
    <> progDesc
         ("Run, test and benchmark challenges from Advent of Code. Available days: "
         ++ availableDays
         )
    )
  cfg <- readConfig $ fromMaybe defConfPath _oConfig
  out <- runExceptT $ case _oMode of
    Run    ro -> void $ mainRun cfg ro
    Submit so -> void $ mainSubmit cfg so
  forOf_ _Left out $ \e -> do
    withColor ANSI.Vivid ANSI.Red $ putStrLn "[ERROR]"
    traverse_ putStrLn e
 where
  availableDays =
    intercalate ", " . map (show . dayInt) . M.keys $ challengeMap

readDay :: ReadM Day
readDay = eitherReader $ \s -> do
  n <- maybe (Left "Invalid day") Right $ readMaybe s
  maybe (Left "Day out of range") Right $ mkDay n

readPart :: ReadM Part
readPart = eitherReader $ \case
  ""  -> Left "No part"
  "a" -> Right Part1
  "b" -> Right Part2
  _   -> Left "Invalid part (not 'a' or 'b')"

parseChallengeSpec :: Parser ChallengeSpec
parseChallengeSpec = do
  d <- argument readDay (metavar "DAY" <> help "Day of challenge (1 - 25)")
  p <- argument readPart (metavar "PART" <> help "Challenge part ('a' or 'b')")
  pure $ ChallengeSpec d p

parseTestSpec :: Parser TestSpec
parseTestSpec = do
  d <- argument
    pDay
    (metavar "DAY" <> help "Day of challenge (1 - 25), or \"all\"")
  p <- optional
    $ argument readPart (metavar "PART" <> help "Challenge part ('a' or 'b')")
  pure $ case d of
    Just d' -> case p of
      Just p' -> TSDayPart (ChallengeSpec d' p')
      Nothing -> TSDayAll d'
    Nothing -> TSAll
 where
  pDay = asum
    [ Nothing <$ maybeReader (guard . (== "all") . map toLower)
    , Just <$> readDay
    ]

parseRun :: Parser RunOpts
parseRun = do
  _roSpec   <- parseTestSpec
  _roActual <-
    fmap not
    . switch
    . mconcat
    $ [ long "skip"
      , short 's'
      , help "Do not run the actual input, but run tests and/or benchmarks"
      ]
  _roTest <-
    switch . mconcat $ [long "test", short 't', help "Run sample tests"]
  _roBench <-
    switch . mconcat $ [long "bench", short 'b', help "Run benchmarks"]
  pure RunOpts { .. }


parseSubmit :: Parser SubmitOpts
parseSubmit = do
  _soSpec <- parseChallengeSpec
  _soTest <-
    fmap not
    . switch
    . mconcat
    $ [ long "skip-tests"
      , short 's'
      , help "Skip running tests before submission"
      ]
  _soForce <-
    switch
    . mconcat
    $ [long "force", short 'f', help "Always submit, even if tests fail"]
  pure SubmitOpts { .. }


parseOpts :: IORef (Maybe (Maybe (Day, String))) -> Parser Opts
parseOpts inputCache = do
  _oConfig <-
    optional
    . strOption
    . mconcat
    $ [ long "config"
      , metavar "PATH"
      , help $ printf "Path to configuration file (default %s)" defConfPath
      ]
  _oMode <-
    subparser
    . mconcat
    $ [ command "run" $ info (Run <$> parseRun <**> helper)
                             (progDesc "Run, test, and benchmark")
      , command "submit" $ info (Submit <$> parseSubmit <**> helper)
                                (progDesc "Test and submit an answer")
      , command "test" $ info (Run <$> parseTest <**> helper)
                              (progDesc "Alias for run --test")
      , command "bench" $ info (Run <$> parseBench <**> helper)
                               (progDesc "Alias for run --bench")
      ]
  pure O { .. }
 where
  parseTest  = fmap (\ro -> ro { _roTest = True }) parseRun
  parseBench = fmap (\ro -> ro { _roBench = True }) parseRun
