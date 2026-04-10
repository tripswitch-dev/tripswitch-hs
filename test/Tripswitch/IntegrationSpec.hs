{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tripswitch.IntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar')
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Test.Hspec

import Tripswitch
import Tripswitch.Client (cBreakerStates)

spec :: Spec
spec = do
  describe "Integration tests" $ do
    mKey <- runIO $ lookupEnv "TRIPSWITCH_API_KEY"
    case mKey of
      Nothing -> do
        it "are gated by TRIPSWITCH_API_KEY env var" $
          pendingWith "TRIPSWITCH_API_KEY not set"
      Just apiKeyStr -> do
        let apiKey = T.pack apiKeyStr
        mProjectID <- runIO $ lookupEnv "TRIPSWITCH_PROJECT_ID"
        let projectID = maybe "test-project" T.pack mProjectID
        mIngestSecret <- runIO $ lookupEnv "TRIPSWITCH_INGEST_SECRET"
        let ingestSecret = maybe "" T.pack mIngestSecret
        mBaseURL <- runIO $ lookupEnv "TRIPSWITCH_BASE_URL"

        let liveCfg = defaultConfig
              { cfgProjectID = projectID
              , cfgApiKey = apiKey
              , cfgIngestSecret = ingestSecret
              , cfgLogger = nullLogger
              }
            liveCfgWithBase = case mBaseURL of
              Nothing -> liveCfg
              Just url -> liveCfg { cfgBaseURL = T.pack url }

        it "SSE connects and populates breaker states" $ do
          withClient liveCfgWithBase $ \client -> do
            -- Wait briefly for SSE to connect and populate states
            threadDelay 3000000 -- 3s
            stats <- getStats client
            ssSSEConnected stats `shouldBe` True

        it "execute passthrough reports metrics" $ do
          withClient liveCfgWithBase { cfgSSEDisabled = True, cfgFlusherDisabled = True, cfgMetaSyncDisabled = True } $ \client -> do
            result <- execute client defaultExecConfig
              { ecRouterID = "integration-test"
              , ecMetrics = Map.singleton "latency" MetricLatency
              } (pure (42 :: Int))
            result `shouldBe` Right 42

        it "getStats reflects breaker cache" $ do
          withClient liveCfgWithBase { cfgSSEDisabled = True, cfgFlusherDisabled = True, cfgMetaSyncDisabled = True } $ \client -> do
            atomically $ modifyTVar' (cBreakerStates client) (Map.insert "test-brk" Closed)
            stats <- getStats client
            ssCachedBreakers stats `shouldBe` 1

        it "getAllStates returns cached states" $ do
          withClient liveCfgWithBase { cfgSSEDisabled = True, cfgFlusherDisabled = True, cfgMetaSyncDisabled = True } $ \client -> do
            atomically $ modifyTVar' (cBreakerStates client) (Map.insert "test-brk" Open)
            states <- getAllStates client
            case states of
              [s] -> bsState s `shouldBe` Open
              _   -> expectationFailure $ "expected 1 state, got " <> show (length states)

        it "close is idempotent" $ do
          client <- newClient liveCfgWithBase { cfgSSEDisabled = True, cfgFlusherDisabled = True, cfgMetaSyncDisabled = True }
          closeClient client
          closeClient client
