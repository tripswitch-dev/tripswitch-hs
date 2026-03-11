{-# LANGUAGE OverloadedStrings #-}

-- | Basic usage of the Tripswitch SDK.
module Main (main) where

import qualified Data.Map.Strict as Map
import Tripswitch

main :: IO ()
main = do
  let cfg =
        defaultConfig
          { cfgProjectID = "proj_123"
          , cfgApiKey = "eb_live_your_key_here"
          }

  withClient cfg $ \client -> do
    result <-
      execute client defaultExecConfig
        { ecBreakers = ["payment-latency"]
        , ecRouterID = "main-router"
        , ecMetrics = Map.singleton "latency" MetricLatency
        , ecTags = Map.singleton "endpoint" "/api/charge"
        }
        databaseCall

    case result of
      Left ErrBreakerOpen ->
        putStrLn "Circuit breaker is open — serving cached response"
      Left (ErrConflictingOptions msg) ->
        putStrLn $ "Config error: " <> show msg
      Left ErrMetadataUnavailable ->
        putStrLn "Metadata not yet loaded"
      Right val ->
        putStrLn $ "Success: " <> show val

databaseCall :: IO Int
databaseCall = do
  putStrLn "Calling database..."
  pure 42
