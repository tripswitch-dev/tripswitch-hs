{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tripswitch.IntegrationSpec (spec) where

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

        it "client lifecycle: create, use, close" $ do
          client <- newClient defaultConfig
            { cfgProjectID = projectID
            , cfgApiKey = apiKey
            , cfgSSEDisabled = True
            , cfgFlusherDisabled = True
            , cfgMetaSyncDisabled = True
            , cfgLogger = nullLogger
            }
          closeClient client

        it "execute passthrough with no breakers" $ do
          withClient defaultConfig
            { cfgProjectID = projectID
            , cfgApiKey = apiKey
            , cfgSSEDisabled = True
            , cfgFlusherDisabled = True
            , cfgMetaSyncDisabled = True
            , cfgLogger = nullLogger
            } $ \client -> do
              result <- execute client defaultExecConfig (pure (42 :: Int))
              result `shouldBe` Right 42

        it "getStats returns initial stats" $ do
          withClient defaultConfig
            { cfgProjectID = projectID
            , cfgApiKey = apiKey
            , cfgSSEDisabled = True
            , cfgFlusherDisabled = True
            , cfgMetaSyncDisabled = True
            , cfgLogger = nullLogger
            } $ \client -> do
              stats <- getStats client
              ssDroppedSamples stats `shouldBe` 0

        it "getAllStates returns empty for fresh client" $ do
          withClient defaultConfig
            { cfgProjectID = projectID
            , cfgApiKey = apiKey
            , cfgSSEDisabled = True
            , cfgFlusherDisabled = True
            , cfgMetaSyncDisabled = True
            , cfgLogger = nullLogger
            } $ \client -> do
              states <- getAllStates client
              length states `shouldBe` 0

        it "close is idempotent" $ do
          client <- newClient defaultConfig
            { cfgProjectID = projectID
            , cfgApiKey = apiKey
            , cfgSSEDisabled = True
            , cfgFlusherDisabled = True
            , cfgMetaSyncDisabled = True
            , cfgLogger = nullLogger
            }
          closeClient client
          closeClient client
