
import AoC
import Control.Monad.Except

main :: IO ()
main = do
  void . runExceptT . mainRun $ (defaultRunOpts TSAll)
    { _roBench = True}