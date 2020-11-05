import CmdStan
import Control.Concurrent.Async
import Data.Maybe
import Control.Exception

main :: IO ()
main = do
  -- Compile the model
  make =<< makeDefaultMakeConfig "data/simple-bernoulli"
  -- Spawn for chains and sample
  outputFiles <- forConcurrently [1..4] $ \chainIndex -> do
    let config = makeDefaultSample "data/simple-bernoulli" chainIndex
    stan "data/simple-bernoulli" config
    pure $ fromMaybe (error "impossible") $ output config

  -- Review the results
  either (throwIO . userError) (putStrLn . unparsed) =<< stansummary (makeDefaultSummaryConfig outputFiles)
