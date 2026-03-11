{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tripswitch.AdminIntegrationSpec (spec) where

import Control.Exception (try)
import Data.Aeson (object, (.=))
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Test.Hspec

import Tripswitch.Admin
import Tripswitch.Admin.Errors
import Tripswitch.Admin.Types

-- ---------------------------------------------------------------------------
-- Admin integration tests — gated by TRIPSWITCH_ADMIN_KEY env var.
--
-- Run with:
--   TRIPSWITCH_ADMIN_KEY=eb_admin_...
--   TRIPSWITCH_PROJECT_ID=proj_...
--   TRIPSWITCH_BASE_URL=http://localhost:4009
--   cabal test --test-option='-m "Admin integration"'
-- ---------------------------------------------------------------------------

data TestEnv = TestEnv
  { teAdminKey :: !T.Text
  , teProjectID :: !T.Text
  , teBaseURL :: !T.Text
  }

loadEnv :: IO (Maybe TestEnv)
loadEnv = do
  mKey <- lookupEnv "TRIPSWITCH_ADMIN_KEY"
  case mKey of
    Nothing -> pure Nothing
    Just key -> do
      mProjectID <- lookupEnv "TRIPSWITCH_PROJECT_ID"
      mBase <- lookupEnv "TRIPSWITCH_BASE_URL"
      pure $ Just TestEnv
        { teAdminKey = T.pack key
        , teProjectID = maybe "" T.pack mProjectID
        , teBaseURL = maybe "https://api.tripswitch.dev" T.pack mBase
        }

mkClient :: TestEnv -> IO AdminClient
mkClient env = newAdminClient defaultAdminConfig
  { acAdminKey = teAdminKey env
  , acBaseURL = teBaseURL env
  }

spec :: Spec
spec = do
  describe "Admin integration tests" $ do
    mEnv <- runIO loadEnv
    case mEnv of
      Nothing -> do
        it "are gated by TRIPSWITCH_ADMIN_KEY env var" $
          pendingWith "TRIPSWITCH_ADMIN_KEY not set"
      Just env -> do

        -- -----------------------------------------------------------------
        -- Projects
        -- -----------------------------------------------------------------

        it "getProject returns the test project" $ do
          ac <- mkClient env
          project <- getProject ac (teProjectID env)
          projID project `shouldBe` teProjectID env

        it "listProjects includes the test project" $ do
          ac <- mkClient env
          resp <- listProjects ac
          let ids = map projID (lprProjects resp)
          ids `shouldSatisfy` elem (teProjectID env)

        -- -----------------------------------------------------------------
        -- Breakers
        -- -----------------------------------------------------------------

        it "listBreakers returns breakers for the project" $ do
          ac <- mkClient env
          resp <- listBreakers ac (teProjectID env)
          length (lbrBreakers resp) `shouldSatisfy` (>= 0)

        it "breaker CRUD lifecycle" $ do
          ac <- mkClient env
          let pid = teProjectID env

          -- Create
          breaker <- createBreaker ac pid (object
            [ "name" .= ("hs-integration-test-breaker" :: T.Text)
            , "metric" .= ("test_metric" :: T.Text)
            , "kind" .= ("error_rate" :: T.Text)
            , "op" .= ("gt" :: T.Text)
            , "threshold" .= (0.5 :: Double)
            , "window_ms" .= (60000 :: Int)
            , "min_count" .= (10 :: Int)
            ])
          brkName breaker `shouldBe` "hs-integration-test-breaker"

          -- Read
          fetched <- getBreaker ac pid (brkID breaker)
          brkName fetched `shouldBe` "hs-integration-test-breaker"

          -- Update
          updated <- updateBreaker ac pid (brkID breaker) (object
            [ "threshold" .= (0.75 :: Double)
            ])
          brkThreshold updated `shouldBe` 0.75

          -- Delete
          deleteBreaker ac pid (brkID breaker)

          -- Verify deletion
          result <- try $ getBreaker ac pid (brkID breaker)
          case result of
            Left err -> isNotFound err `shouldBe` True
            Right _ -> expectationFailure "expected NotFound after deletion"

        -- -----------------------------------------------------------------
        -- Routers
        -- -----------------------------------------------------------------

        it "listRouters returns routers for the project" $ do
          ac <- mkClient env
          resp <- listRouters ac (teProjectID env)
          length (lrrRouters resp) `shouldSatisfy` (>= 0)

        -- -----------------------------------------------------------------
        -- Notification Channels
        -- -----------------------------------------------------------------

        it "listNotificationChannels returns channels" $ do
          ac <- mkClient env
          resp <- listNotificationChannels ac (teProjectID env)
          length (lcrChannels resp) `shouldSatisfy` (>= 0)

        -- -----------------------------------------------------------------
        -- Events
        -- -----------------------------------------------------------------

        it "listEvents returns events" $ do
          ac <- mkClient env
          resp <- listEvents ac (teProjectID env)
          length (lerEvents resp) `shouldSatisfy` (>= 0)

        -- -----------------------------------------------------------------
        -- Project Keys
        -- -----------------------------------------------------------------

        it "listProjectKeys returns keys" $ do
          ac <- mkClient env
          resp <- listProjectKeys ac (teProjectID env)
          length (lkrKeys resp) `shouldSatisfy` (>= 0)

        -- -----------------------------------------------------------------
        -- Error handling
        -- -----------------------------------------------------------------

        it "getProject with bad ID returns NotFound" $ do
          ac <- mkClient env
          result <- try $ getProject ac "nonexistent-project-id"
          case result of
            Left err -> isNotFound err `shouldBe` True
            Right _ -> expectationFailure "expected NotFound"
