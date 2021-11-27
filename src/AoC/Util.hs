module AoC.Util
  ( strip
  , stripNewlines
  , eitherToMaybe
  , maybeToEither
  , withColor
  ) where

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.Text                     as T
import qualified System.Console.ANSI           as ANSI

-- | Strip trailing and leading whitepsace
strip :: String -> String
strip = T.unpack . T.strip . T.pack

-- | Strip trailing newlines
stripNewlines :: String -> String
stripNewlines = reverse . dropWhile (== '\n') . reverse

-- | Convert an 'Either' into a 'Maybe' (or any other 'Alternative' instance),
-- dropping the error value.
eitherToMaybe :: Alternative m => Either e a -> m a
eitherToMaybe = either (const empty) pure

-- | Conver a 'Maybe' into an 'Either' (or any other 'MonadError' instance),
-- using the provided error value if necessary.
maybeToEither :: MonadError e m => e -> Maybe a -> m a
maybeToEither e = maybe (throwError e) pure

-- | Run som IO with the specified foreground colour and intensity.
withColor :: ANSI.ColorIntensity -> ANSI.Color -> IO () -> IO ()
withColor ci c io = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ci c]
  io
  ANSI.setSGR [ANSI.Reset]
