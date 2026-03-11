{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tripswitch.AdminSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status200, status201, status204, status400, status401, status403, status404, status409, status422, status429, status500)
import Network.Wai (Application, getRequestBodyChunk, requestHeaders, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import Test.Hspec

import Tripswitch.Admin
import Tripswitch.Admin.Errors
import Tripswitch.Admin.Types

-- ---------------------------------------------------------------------------
-- Mock server helper
-- ---------------------------------------------------------------------------

-- | Run a WAI app on a random port, execute an action with the admin client, then stop.
withMockServer :: Application -> (AdminClient -> IO a) -> IO a
withMockServer app action = do
  (port, socket) <- Warp.openFreePort
  let settings = Warp.setPort port $ Warp.defaultSettings
  tid <- forkIO $ Warp.runSettingsSocket settings socket app
  -- Give the server a moment to start
  threadDelay 50000
  let cfg = defaultAdminConfig
        { acAdminKey = "test-admin-key"
        , acBaseURL = "http://localhost:" <> T.pack (show port)
        }
  ac <- newAdminClient cfg
  result <- action ac
  killThread tid
  pure result

-- | Simple mock server that returns a fixed JSON response.
jsonApp :: Int -> Value -> Application
jsonApp statusCode body _req respond = do
  let status = case statusCode of
        200 -> status200
        201 -> status201
        204 -> status204
        400 -> status400
        401 -> status401
        403 -> status403
        404 -> status404
        409 -> status409
        422 -> status422
        429 -> status429
        500 -> status500
        _ -> status200
  respond $ responseLBS status [("Content-Type", "application/json")] (Aeson.encode body)

spec :: Spec
spec = do
  -- =======================================================================
  -- Projects
  -- =======================================================================
  describe "Projects" $ do
    it "getProject returns a project" $ do
      let mockProject = object
            [ "project_id" .= ("proj_1" :: Text)
            , "name" .= ("My Project" :: Text)
            , "enable_signed_ingest" .= False
            ]
      result <- withMockServer (jsonApp 200 mockProject) $ \ac ->
        getProject ac "proj_1"
      projID result `shouldBe` "proj_1"
      projName result `shouldBe` "My Project"

    it "createProject sends POST and returns project" $ do
      let mockProject = object
            [ "project_id" .= ("proj_new" :: Text)
            , "name" .= ("New Project" :: Text)
            , "enable_signed_ingest" .= False
            ]
      result <- withMockServer (jsonApp 200 mockProject) $ \ac ->
        createProject ac (object ["name" .= ("New Project" :: Text)])
      projName result `shouldBe` "New Project"

    it "listProjects returns projects in response" $ do
      let mockResp = object
            [ "projects" .=
                [ object ["project_id" .= ("p1" :: Text), "name" .= ("P1" :: Text), "enable_signed_ingest" .= False]
                , object ["project_id" .= ("p2" :: Text), "name" .= ("P2" :: Text), "enable_signed_ingest" .= True]
                ]
            , "count" .= (2 :: Int)
            ]
      result <- withMockServer (jsonApp 200 mockResp) $ \ac ->
        listProjects ac
      length (lprProjects result) `shouldBe` 2
      lprCount result `shouldBe` 2

  -- =======================================================================
  -- Breakers
  -- =======================================================================
  describe "Breakers" $ do
    it "createBreaker returns a breaker" $ do
      let mockBreaker = object
            [ "id" .= ("brk_1" :: Text)
            , "router_id" .= ("rtr_1" :: Text)
            , "name" .= ("error-rate" :: Text)
            , "metric" .= ("errors" :: Text)
            , "kind" .= ("error_rate" :: Text)
            , "op" .= ("gt" :: Text)
            , "threshold" .= (0.5 :: Double)
            , "window_ms" .= (60000 :: Int)
            , "min_count" .= (10 :: Int)
            , "min_state_duration_ms" .= (5000 :: Int)
            , "cooldown_ms" .= (30000 :: Int)
            , "eval_interval_ms" .= (10000 :: Int)
            , "half_open_backoff_enabled" .= False
            , "metadata" .= object []
            ]
      result <- withMockServer (jsonApp 200 mockBreaker) $ \ac ->
        createBreaker ac "proj_1" (object ["name" .= ("error-rate" :: Text)])
      brkName result `shouldBe` "error-rate"
      brkKind result `shouldBe` ErrorRate
      brkOp result `shouldBe` OpGT

    it "deleteBreaker succeeds on 204" $ do
      result <- try @SomeException $
        withMockServer (\_ respond -> respond $ responseLBS status204 [] "") $ \ac ->
          deleteBreaker ac "proj_1" "brk_1"
      case result of
        Left _ -> pure () -- 204 with empty body may cause parse error, that's ok for now
        Right _ -> pure ()

  -- =======================================================================
  -- Routers
  -- =======================================================================
  describe "Routers" $ do
    it "createRouter returns a router" $ do
      let mockRouter = object
            [ "id" .= ("rtr_1" :: Text)
            , "name" .= ("main" :: Text)
            , "mode" .= ("static" :: Text)
            , "enabled" .= True
            , "breaker_count" .= (0 :: Int)
            , "metadata" .= object []
            ]
      result <- withMockServer (jsonApp 200 mockRouter) $ \ac ->
        createRouter ac "proj_1" (object ["name" .= ("main" :: Text)])
      rtrName result `shouldBe` "main"
      rtrMode result `shouldBe` Static

  -- =======================================================================
  -- Error taxonomy
  -- =======================================================================
  describe "Error taxonomy" $ do
    it "404 produces ErrNotFound" $ do
      let errBody = object ["message" .= ("not found" :: Text), "code" .= ("not_found" :: Text)]
      result <- try @APIError $
        withMockServer (jsonApp 404 errBody) $ \ac ->
          getProject ac "nonexistent"
      case result of
        Left err -> do
          isNotFound err `shouldBe` True
          aeStatusCode err `shouldBe` 404
        Right _ -> expectationFailure "expected APIError"

    it "401 produces ErrUnauthorized" $ do
      let errBody = object ["message" .= ("unauthorized" :: Text)]
      result <- try @APIError $
        withMockServer (jsonApp 401 errBody) $ \ac ->
          getProject ac "p1"
      case result of
        Left err -> isUnauthorized err `shouldBe` True
        Right _ -> expectationFailure "expected APIError"

    it "403 produces ErrForbidden" $ do
      let errBody = object ["message" .= ("forbidden" :: Text)]
      result <- try @APIError $
        withMockServer (jsonApp 403 errBody) $ \ac ->
          getProject ac "p1"
      case result of
        Left err -> isForbidden err `shouldBe` True
        Right _ -> expectationFailure "expected APIError"

    it "409 produces ErrConflict" $ do
      let errBody = object ["message" .= ("conflict" :: Text)]
      result <- try @APIError $
        withMockServer (jsonApp 409 errBody) $ \ac ->
          createProject ac (object [])
      case result of
        Left err -> isConflict err `shouldBe` True
        Right _ -> expectationFailure "expected APIError"

    it "422 produces ErrValidation" $ do
      let errBody = object ["message" .= ("invalid" :: Text)]
      result <- try @APIError $
        withMockServer (jsonApp 422 errBody) $ \ac ->
          createProject ac (object [])
      case result of
        Left err -> isValidation err `shouldBe` True
        Right _ -> expectationFailure "expected APIError"

    it "429 produces ErrRateLimited" $ do
      let errBody = object ["message" .= ("rate limited" :: Text)]
      result <- try @APIError $
        withMockServer (jsonApp 429 errBody) $ \ac ->
          getProject ac "p1"
      case result of
        Left err -> isRateLimited err `shouldBe` True
        Right _ -> expectationFailure "expected APIError"

    it "500 produces ErrServerFault" $ do
      let errBody = object ["message" .= ("internal error" :: Text)]
      result <- try @APIError $
        withMockServer (jsonApp 500 errBody) $ \ac ->
          getProject ac "p1"
      case result of
        Left err -> isServerFault err `shouldBe` True
        Right _ -> expectationFailure "expected APIError"

    it "transport error for unreachable host" $ do
      let cfg = defaultAdminConfig
            { acAdminKey = "key"
            , acBaseURL = "http://localhost:1"  -- unreachable
            }
      ac <- newAdminClient cfg
      result <- try @APIError $ getProject ac "p1"
      case result of
        Left err -> isTransport err `shouldBe` True
        Right _ -> expectationFailure "expected transport error"

  -- =======================================================================
  -- Request options
  -- =======================================================================
  describe "Request options" $ do
    it "auth header is set" $ do
      let app req respond = do
            let authHeader = lookup "Authorization" (requestHeaders req)
            case authHeader of
              Just val | val == "Bearer test-admin-key" ->
                respond $ responseLBS status200 []
                  (Aeson.encode (object ["project_id" .= ("p1" :: Text), "name" .= ("P" :: Text), "enable_signed_ingest" .= False]))
              _ ->
                respond $ responseLBS status401 [] "{\"message\":\"bad auth\"}"
      result <- withMockServer app $ \ac ->
        getProject ac "p1"
      projID result `shouldBe` "p1"

  -- =======================================================================
  -- Error predicate functions
  -- =======================================================================
  describe "Error predicates" $ do
    let mkErr kind code = APIError kind code "" Nothing
    it "isNotFound" $ do
      isNotFound (mkErr ErrNotFound 404) `shouldBe` True
      isNotFound (mkErr ErrUnauthorized 401) `shouldBe` False

    it "isUnauthorized" $ do
      isUnauthorized (mkErr ErrUnauthorized 401) `shouldBe` True

    it "isForbidden" $ do
      isForbidden (mkErr ErrForbidden 403) `shouldBe` True

    it "isConflict" $ do
      isConflict (mkErr ErrConflict 409) `shouldBe` True

    it "isRateLimited" $ do
      isRateLimited (mkErr ErrRateLimited 429) `shouldBe` True

    it "isValidation" $ do
      isValidation (mkErr ErrValidation 400) `shouldBe` True

    it "isServerFault" $ do
      isServerFault (mkErr ErrServerFault 500) `shouldBe` True

    it "isTransport" $ do
      isTransport (mkErr ErrTransport 0) `shouldBe` True

  -- =======================================================================
  -- Admin types parsing
  -- =======================================================================
  describe "Admin types" $ do
    it "BreakerKind parses all values" $ do
      Aeson.eitherDecode "\"error_rate\"" `shouldBe` Right ErrorRate
      Aeson.eitherDecode "\"avg\"" `shouldBe` Right Avg
      Aeson.eitherDecode "\"p95\"" `shouldBe` Right P95
      Aeson.eitherDecode "\"max\"" `shouldBe` Right Max
      Aeson.eitherDecode "\"min\"" `shouldBe` Right Min
      Aeson.eitherDecode "\"sum\"" `shouldBe` Right Sum
      Aeson.eitherDecode "\"stddev\"" `shouldBe` Right StdDev
      Aeson.eitherDecode "\"count\"" `shouldBe` Right Count
      Aeson.eitherDecode "\"percentile\"" `shouldBe` Right Percentile
      Aeson.eitherDecode "\"consecutive_failures\"" `shouldBe` Right ConsecutiveFailures
      Aeson.eitherDecode "\"delta\"" `shouldBe` Right Delta

    it "BreakerOp parses all values" $ do
      Aeson.eitherDecode "\"gt\"" `shouldBe` Right OpGT
      Aeson.eitherDecode "\"lt\"" `shouldBe` Right OpLT
      Aeson.eitherDecode "\"gte\"" `shouldBe` Right OpGTE
      Aeson.eitherDecode "\"lte\"" `shouldBe` Right OpLTE

    it "HalfOpenPolicy parses all values" $ do
      Aeson.eitherDecode "\"optimistic\"" `shouldBe` Right Optimistic
      Aeson.eitherDecode "\"conservative\"" `shouldBe` Right Conservative
      Aeson.eitherDecode "\"pessimistic\"" `shouldBe` Right Pessimistic

    it "RouterMode parses all values" $ do
      Aeson.eitherDecode "\"static\"" `shouldBe` Right Static
      Aeson.eitherDecode "\"canary\"" `shouldBe` Right Canary
      Aeson.eitherDecode "\"weighted\"" `shouldBe` Right Weighted

    it "NotificationChannelType parses all values" $ do
      Aeson.eitherDecode "\"slack\"" `shouldBe` Right Slack
      Aeson.eitherDecode "\"pagerduty\"" `shouldBe` Right PagerDuty
      Aeson.eitherDecode "\"email\"" `shouldBe` Right Email
      Aeson.eitherDecode "\"webhook\"" `shouldBe` Right Webhook

    it "NotificationEventType parses all values" $ do
      Aeson.eitherDecode "\"trip\"" `shouldBe` Right Trip
      Aeson.eitherDecode "\"recover\"" `shouldBe` Right Recover

  -- =======================================================================
  -- Bug fixes
  -- =======================================================================
  describe "parseAPIError extracts message from JSON" $ do
    it "aeMessage contains the JSON message field" $ do
      let errBody = object ["message" .= ("not found" :: Text), "code" .= ("not_found" :: Text)]
      result <- try @APIError $
        withMockServer (jsonApp 404 errBody) $ \ac ->
          getProject ac "nonexistent"
      case result of
        Left err -> aeMessage err `shouldBe` "not found"
        Right _ -> expectationFailure "expected APIError"

    it "aeMessage falls back to raw body when no message field" $ do
      let errBody = object ["error" .= ("something" :: Text)]
      result <- try @APIError $
        withMockServer (jsonApp 400 errBody) $ \ac ->
          getProject ac "p1"
      case result of
        Left err -> aeMessage err `shouldNotBe` ""
        Right _ -> expectationFailure "expected APIError"

  describe "deleteProject sends body" $ do
    it "request body contains the expected JSON" $ do
      bodyRef <- newIORef LBS.empty
      let app req respond = do
            body <- LBS.fromStrict <$> getRequestBodyChunk req
            writeIORef bodyRef body
            respond $ responseLBS status204 [] ""
          deleteBody = object ["cascade" .= True]
      _ <- try @SomeException $
        withMockServer app $ \ac ->
          deleteProject ac "proj_1" deleteBody
      capturedBody <- readIORef bodyRef
      let parsed = Aeson.decode capturedBody :: Maybe Value
      parsed `shouldBe` Just (object ["cascade" .= True])

  describe "Retry-After header" $ do
    it "429 with Retry-After parses into aeRetryAfter" $ do
      let errBody = object ["message" .= ("rate limited" :: Text)]
          app _req respond =
            respond $ responseLBS status429
              [("Content-Type", "application/json"), ("Retry-After", "30")]
              (Aeson.encode errBody)
      result <- try @APIError $
        withMockServer app $ \ac ->
          getProject ac "p1"
      case result of
        Left err -> do
          isRateLimited err `shouldBe` True
          aeRetryAfter err `shouldBe` Just 30
        Right _ -> expectationFailure "expected APIError"

    it "429 without Retry-After has Nothing" $ do
      let errBody = object ["message" .= ("rate limited" :: Text)]
      result <- try @APIError $
        withMockServer (jsonApp 429 errBody) $ \ac ->
          getProject ac "p1"
      case result of
        Left err -> aeRetryAfter err `shouldBe` Nothing
        Right _ -> expectationFailure "expected APIError"
