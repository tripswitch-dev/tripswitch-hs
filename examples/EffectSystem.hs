{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Using Tripswitch with the effectful effect system.
module Main (main) where

import qualified Data.Map.Strict as Map
import Effectful (Eff, IOE, runEff, (:>))

import Tripswitch
import Tripswitch.Effect (CircuitBreaker, cbExecute)
import Tripswitch.Effect.Interpreters (runCircuitBreakerIO)

-- | A function that declares circuit breaking in its type.
fetchData :: (CircuitBreaker :> es, IOE :> es) => Eff es (Either TripSwitchError String)
fetchData =
  cbExecute defaultExecConfig
    { ecBreakers = ["data-service"]
    , ecRouterID = "main-router"
    , ecMetrics = Map.singleton "latency" MetricLatency
    }
    (pure "data from service")

main :: IO ()
main = do
  let cfg =
        defaultConfig
          { cfgProjectID = "proj_123"
          , cfgSSEDisabled = True
          , cfgFlusherDisabled = True
          , cfgMetaSyncDisabled = True
          }

  withClient cfg $ \client -> do
    result <- runEff $ runCircuitBreakerIO client fetchData
    case result of
      Left ErrBreakerOpen -> putStrLn "Breaker open!"
      Right val -> putStrLn $ "Got: " <> val
