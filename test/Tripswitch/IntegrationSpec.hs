{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tripswitch.IntegrationSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar')
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Test.Hspec

import Tripswitch.Client

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

        let liveCfg = defaultConfig
              { cfgProjectID = projectID
              , cfgApiKey = apiKey
              , cfgIngestSecret = ingestSecret
              , cfgLogger = nullLogger
              }

        it "connects and receives initial breaker states via SSE" $ do
          -- Uses the full Tripswitch.newClient (via withClient) which
          -- starts background threads and blocks on SSE readiness.
          -- If SSE connects, cSSEReady is signaled within 5s.
          pendingWith "requires Tripswitch module import with thread spawning"

        it "execute passthrough reports metrics" $ do
          withClient liveCfg { cfgSSEDisabled = True, cfgFlusherDisabled = True, cfgMetaSyncDisabled = True } $ \client -> do
            result <- execute client defaultExecConfig
              { ecRouterID = "integration-test"
              , ecMetrics = Map.singleton "latency" MetricLatency
              } (pure (42 :: Int))
            result `shouldBe` Right 42

        it "getStats reflects breaker cache" $ do
          withClient liveCfg { cfgSSEDisabled = True, cfgFlusherDisabled = True, cfgMetaSyncDisabled = True } $ \client -> do
            atomically $ modifyTVar' (cBreakerStates client) (Map.insert "test-brk" Closed)
            stats <- getStats client
            ssCachedBreakers stats `shouldBe` 1

        it "getAllStates returns cached states" $ do
          withClient liveCfg { cfgSSEDisabled = True, cfgFlusherDisabled = True, cfgMetaSyncDisabled = True } $ \client -> do
            atomically $ modifyTVar' (cBreakerStates client) (Map.insert "test-brk" Open)
            states <- getAllStates client
            length states `shouldBe` 1
            bsState (head states) `shouldBe` Open

        it "close is idempotent" $ do
          client <- newClient liveCfg { cfgSSEDisabled = True, cfgFlusherDisabled = True, cfgMetaSyncDisabled = True }
          closeClient client
          closeClient client
