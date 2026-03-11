{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Tripswitch.Admin
-- Description : Admin API client for managing TripSwitch resources.
--
-- Create an 'AdminClient' and use it to manage projects, breakers,
-- routers, notification channels, and events.
module Tripswitch.Admin
  ( -- * Client
    AdminClient (..)
  , AdminConfig (..)
  , defaultAdminConfig
  , newAdminClient

    -- * Request Configuration
  , RequestConfig (..)
  , defaultRequestConfig

    -- * Projects
  , listProjects
  , createProject
  , getProject
  , updateProject
  , deleteProject
  , rotateIngestSecret

    -- * Breakers
  , listBreakers
  , createBreaker
  , getBreaker
  , updateBreaker
  , deleteBreaker
  , syncBreakers
  , getBreakerState
  , batchGetBreakerStates

    -- * Routers
  , listRouters
  , createRouter
  , getRouter
  , updateRouter
  , deleteRouter
  , linkBreaker
  , unlinkBreaker

    -- * Notification Channels
  , listNotificationChannels
  , createNotificationChannel
  , getNotificationChannel
  , updateNotificationChannel
  , deleteNotificationChannel
  , testNotificationChannel

    -- * Events
  , listEvents

    -- * Project Keys
  , listProjectKeys
  , createProjectKey
  , deleteProjectKey

    -- * Metadata
  , updateBreakerMetadata
  , updateRouterMetadata
  ) where

import qualified Data.CaseInsensitive as CI
import Control.Exception (Exception (..), SomeException, catch, throwIO, try)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, eitherDecode, encode, object, (.:), (.=))
import Data.Aeson qualified as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (..)
  , Response (..)
  , httpLbs
  , newManager
  , parseRequest
  , responseStatus
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

import Tripswitch.Admin.Errors (APIError (..), AdminErrorKind (..))
import Tripswitch.Admin.Types

-- ---------------------------------------------------------------------------
-- Admin Client
-- ---------------------------------------------------------------------------

-- | Configuration for the admin client.
data AdminConfig = AdminConfig
  { acAdminKey :: !Text
  , acBaseURL :: !Text
  }

-- | Default admin config.
defaultAdminConfig :: AdminConfig
defaultAdminConfig =
  AdminConfig
    { acAdminKey = ""
    , acBaseURL = "https://api.tripswitch.dev"
    }

-- | Admin API client.
data AdminClient = AdminClient
  { adminConfig :: !AdminConfig
  , adminManager :: !Manager
  }

-- | Create a new admin client.
newAdminClient :: AdminConfig -> IO AdminClient
newAdminClient cfg = do
  mgr <- newManager tlsManagerSettings
  pure AdminClient {adminConfig = cfg, adminManager = mgr}

-- ---------------------------------------------------------------------------
-- Request Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for individual admin API requests.
data RequestConfig = RequestConfig
  { rcIdempotencyKey :: !(Maybe Text)
  , rcTimeout :: !(Maybe Int)
  , rcExtraHeaders :: !(Map Text Text)
  , rcRequestID :: !(Maybe Text)
  }

-- | Default request configuration with no options set.
defaultRequestConfig :: RequestConfig
defaultRequestConfig =
  RequestConfig
    { rcIdempotencyKey = Nothing
    , rcTimeout = Nothing
    , rcExtraHeaders = Map.empty
    , rcRequestID = Nothing
    }

-- ---------------------------------------------------------------------------
-- Internal HTTP helper
-- ---------------------------------------------------------------------------

doRequest :: (FromJSON a) => AdminClient -> Text -> Text -> Maybe Value -> RequestConfig -> IO a
doRequest ac httpMethod path mBody rc = do
  let cfg = adminConfig ac
      url = T.unpack (acBaseURL cfg) <> T.unpack path
  req <- parseRequest url
  let baseReq =
        req
          { method = TE.encodeUtf8 httpMethod
          , requestHeaders =
              [ ("Authorization", "Bearer " <> TE.encodeUtf8 (acAdminKey cfg))
              , ("Content-Type", "application/json")
              ]
          , requestBody = maybe (RequestBodyBS "") (RequestBodyLBS . encode) mBody
          }
      finalReq = applyRequestConfig rc baseReq
  result <- try $ httpLbs finalReq (adminManager ac)
  case result of
    Left (exc :: SomeException) ->
      throwIO $ APIError ErrTransport 0 (T.pack (show exc)) Nothing
    Right resp -> do
      let status = statusCode (responseStatus resp)
      if status >= 200 && status < 300
        then case eitherDecode (responseBody resp) of
          Left err -> throwIO $ APIError ErrValidation status (T.pack err) Nothing
          Right val -> pure val
        else throwIO $ parseAPIError status resp

doRequestNoBody :: AdminClient -> Text -> Text -> RequestConfig -> IO ()
doRequestNoBody ac httpMethod path rc = do
  let cfg = adminConfig ac
      url = T.unpack (acBaseURL cfg) <> T.unpack path
  req <- parseRequest url
  let baseReq =
        req
          { method = TE.encodeUtf8 httpMethod
          , requestHeaders =
              [ ("Authorization", "Bearer " <> TE.encodeUtf8 (acAdminKey cfg))
              , ("Content-Type", "application/json")
              ]
          }
      finalReq = applyRequestConfig rc baseReq
  result <- try $ httpLbs finalReq (adminManager ac)
  case result of
    Left (exc :: SomeException) ->
      throwIO $ APIError ErrTransport 0 (T.pack (show exc)) Nothing
    Right resp -> do
      let status = statusCode (responseStatus resp)
      if status >= 200 && status < 300
        then pure ()
        else throwIO $ parseAPIError status resp

applyRequestConfig :: RequestConfig -> Request -> Request
applyRequestConfig rc req =
  let hdrs0 = requestHeaders req
      hdrs1 = case rcIdempotencyKey rc of
        Nothing -> hdrs0
        Just k -> ("Idempotency-Key", TE.encodeUtf8 k) : hdrs0
      hdrs2 = case rcRequestID rc of
        Nothing -> hdrs1
        Just rid -> ("X-Request-ID", TE.encodeUtf8 rid) : hdrs1
      hdrs3 = Map.foldlWithKey'
        (\hs k v -> (CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v) : hs)
        hdrs2
        (rcExtraHeaders rc)
   in req {requestHeaders = hdrs3}

parseAPIError :: Int -> Response LBS.ByteString -> APIError
parseAPIError status resp =
  let msg = case eitherDecode (responseBody resp) of
        Right (Aeson.Object o) -> case Map.lookup "message" (Map.fromList []) of
          _ -> T.pack $ show (responseBody resp)
        _ -> T.pack $ show (responseBody resp)
      kind = case status of
        400 -> ErrValidation
        401 -> ErrUnauthorized
        403 -> ErrForbidden
        404 -> ErrNotFound
        409 -> ErrConflict
        422 -> ErrValidation
        429 -> ErrRateLimited
        _ | status >= 500 -> ErrServerFault
        _ -> ErrValidation
      retryAfter = Nothing -- Could parse Retry-After header
   in APIError kind status msg retryAfter

-- ---------------------------------------------------------------------------
-- Projects
-- ---------------------------------------------------------------------------

-- | List all projects.
listProjects :: AdminClient -> RequestConfig -> IO [Project]
listProjects ac opts = doRequest ac "GET" "/v1/projects" Nothing opts

-- | Create a project.
createProject :: AdminClient -> Value -> RequestConfig -> IO Project
createProject ac body opts = doRequest ac "POST" "/v1/projects" (Just body) opts

-- | Get a project by ID.
getProject :: AdminClient -> Text -> RequestConfig -> IO Project
getProject ac pid opts = doRequest ac "GET" ("/v1/projects/" <> pid) Nothing opts

-- | Update a project.
updateProject :: AdminClient -> Text -> Value -> RequestConfig -> IO Project
updateProject ac pid body opts = doRequest ac "PATCH" ("/v1/projects/" <> pid) (Just body) opts

-- | Delete a project.
deleteProject :: AdminClient -> Text -> Value -> RequestConfig -> IO ()
deleteProject ac pid body opts = doRequestNoBody ac "DELETE" ("/v1/projects/" <> pid) opts

-- | Rotate a project's ingest secret.
rotateIngestSecret :: AdminClient -> Text -> RequestConfig -> IO Value
rotateIngestSecret ac pid opts = doRequest ac "POST" ("/v1/projects/" <> pid <> "/ingest_secret/rotate") Nothing opts

-- ---------------------------------------------------------------------------
-- Breakers
-- ---------------------------------------------------------------------------

-- | List breakers for a project.
listBreakers :: AdminClient -> Text -> RequestConfig -> IO [Breaker]
listBreakers ac pid opts = doRequest ac "GET" ("/v1/projects/" <> pid <> "/breakers") Nothing opts

-- | Create a breaker.
createBreaker :: AdminClient -> Text -> Value -> RequestConfig -> IO Breaker
createBreaker ac pid body opts = doRequest ac "POST" ("/v1/projects/" <> pid <> "/breakers") (Just body) opts

-- | Get a breaker by ID.
getBreaker :: AdminClient -> Text -> Text -> RequestConfig -> IO Breaker
getBreaker ac pid bid opts = doRequest ac "GET" ("/v1/projects/" <> pid <> "/breakers/" <> bid) Nothing opts

-- | Update a breaker.
updateBreaker :: AdminClient -> Text -> Text -> Value -> RequestConfig -> IO Breaker
updateBreaker ac pid bid body opts = doRequest ac "PATCH" ("/v1/projects/" <> pid <> "/breakers/" <> bid) (Just body) opts

-- | Delete a breaker.
deleteBreaker :: AdminClient -> Text -> Text -> RequestConfig -> IO ()
deleteBreaker ac pid bid opts = doRequestNoBody ac "DELETE" ("/v1/projects/" <> pid <> "/breakers/" <> bid) opts

-- | Sync (bulk replace) breakers.
syncBreakers :: AdminClient -> Text -> Value -> RequestConfig -> IO [Breaker]
syncBreakers ac pid body opts = doRequest ac "PUT" ("/v1/projects/" <> pid <> "/breakers") (Just body) opts

-- | Get a breaker's state.
getBreakerState :: AdminClient -> Text -> Text -> RequestConfig -> IO AdminBreakerState
getBreakerState ac pid bid opts = doRequest ac "GET" ("/v1/projects/" <> pid <> "/breakers/" <> bid <> "/state") Nothing opts

-- | Batch get breaker states.
batchGetBreakerStates :: AdminClient -> Text -> Value -> RequestConfig -> IO [AdminBreakerState]
batchGetBreakerStates ac pid body opts = doRequest ac "POST" ("/v1/projects/" <> pid <> "/breakers/state:batch") (Just body) opts

-- ---------------------------------------------------------------------------
-- Routers
-- ---------------------------------------------------------------------------

-- | List routers for a project.
listRouters :: AdminClient -> Text -> RequestConfig -> IO [Router]
listRouters ac pid opts = doRequest ac "GET" ("/v1/projects/" <> pid <> "/routers") Nothing opts

-- | Create a router.
createRouter :: AdminClient -> Text -> Value -> RequestConfig -> IO Router
createRouter ac pid body opts = doRequest ac "POST" ("/v1/projects/" <> pid <> "/routers") (Just body) opts

-- | Get a router by ID.
getRouter :: AdminClient -> Text -> Text -> RequestConfig -> IO Router
getRouter ac pid rid opts = doRequest ac "GET" ("/v1/projects/" <> pid <> "/routers/" <> rid) Nothing opts

-- | Update a router.
updateRouter :: AdminClient -> Text -> Text -> Value -> RequestConfig -> IO Router
updateRouter ac pid rid body opts = doRequest ac "PATCH" ("/v1/projects/" <> pid <> "/routers/" <> rid) (Just body) opts

-- | Delete a router.
deleteRouter :: AdminClient -> Text -> Text -> RequestConfig -> IO ()
deleteRouter ac pid rid opts = doRequestNoBody ac "DELETE" ("/v1/projects/" <> pid <> "/routers/" <> rid) opts

-- | Link a breaker to a router.
linkBreaker :: AdminClient -> Text -> Text -> Value -> RequestConfig -> IO Value
linkBreaker ac pid rid body opts = doRequest ac "POST" ("/v1/projects/" <> pid <> "/routers/" <> rid <> "/breakers") (Just body) opts

-- | Unlink a breaker from a router.
unlinkBreaker :: AdminClient -> Text -> Text -> Text -> RequestConfig -> IO ()
unlinkBreaker ac pid rid bid opts = doRequestNoBody ac "DELETE" ("/v1/projects/" <> pid <> "/routers/" <> rid <> "/breakers/" <> bid) opts

-- ---------------------------------------------------------------------------
-- Notification Channels
-- ---------------------------------------------------------------------------

-- | List notification channels.
listNotificationChannels :: AdminClient -> Text -> RequestConfig -> IO [NotificationChannel]
listNotificationChannels ac pid opts = doRequest ac "GET" ("/v1/projects/" <> pid <> "/notification-channels") Nothing opts

-- | Create a notification channel.
createNotificationChannel :: AdminClient -> Text -> Value -> RequestConfig -> IO NotificationChannel
createNotificationChannel ac pid body opts = doRequest ac "POST" ("/v1/projects/" <> pid <> "/notification-channels") (Just body) opts

-- | Get a notification channel.
getNotificationChannel :: AdminClient -> Text -> Text -> RequestConfig -> IO NotificationChannel
getNotificationChannel ac pid cid opts = doRequest ac "GET" ("/v1/projects/" <> pid <> "/notification-channels/" <> cid) Nothing opts

-- | Update a notification channel.
updateNotificationChannel :: AdminClient -> Text -> Text -> Value -> RequestConfig -> IO NotificationChannel
updateNotificationChannel ac pid cid body opts = doRequest ac "PATCH" ("/v1/projects/" <> pid <> "/notification-channels/" <> cid) (Just body) opts

-- | Delete a notification channel.
deleteNotificationChannel :: AdminClient -> Text -> Text -> RequestConfig -> IO ()
deleteNotificationChannel ac pid cid opts = doRequestNoBody ac "DELETE" ("/v1/projects/" <> pid <> "/notification-channels/" <> cid) opts

-- | Test a notification channel.
testNotificationChannel :: AdminClient -> Text -> Text -> RequestConfig -> IO ()
testNotificationChannel ac pid cid opts = doRequestNoBody ac "POST" ("/v1/projects/" <> pid <> "/notification-channels/" <> cid <> "/test") opts

-- ---------------------------------------------------------------------------
-- Events
-- ---------------------------------------------------------------------------

-- | List events for a project.
listEvents :: AdminClient -> Text -> RequestConfig -> IO [Event]
listEvents ac pid opts = doRequest ac "GET" ("/v1/projects/" <> pid <> "/events") Nothing opts

-- ---------------------------------------------------------------------------
-- Project Keys
-- ---------------------------------------------------------------------------

-- | List project keys.
listProjectKeys :: AdminClient -> Text -> RequestConfig -> IO [ProjectKey]
listProjectKeys ac pid opts = doRequest ac "GET" ("/v1/projects/" <> pid <> "/keys") Nothing opts

-- | Create a project key.
createProjectKey :: AdminClient -> Text -> Value -> RequestConfig -> IO ProjectKey
createProjectKey ac pid body opts = doRequest ac "POST" ("/v1/projects/" <> pid <> "/keys") (Just body) opts

-- | Delete a project key.
deleteProjectKey :: AdminClient -> Text -> Text -> RequestConfig -> IO ()
deleteProjectKey ac pid kid opts = doRequestNoBody ac "DELETE" ("/v1/projects/" <> pid <> "/keys/" <> kid) opts

-- ---------------------------------------------------------------------------
-- Metadata
-- ---------------------------------------------------------------------------

-- | Update breaker metadata.
updateBreakerMetadata :: AdminClient -> Text -> Text -> Value -> RequestConfig -> IO Value
updateBreakerMetadata ac pid bid body opts = doRequest ac "PATCH" ("/v1/projects/" <> pid <> "/breakers/" <> bid <> "/metadata") (Just body) opts

-- | Update router metadata.
updateRouterMetadata :: AdminClient -> Text -> Text -> Value -> RequestConfig -> IO Value
updateRouterMetadata ac pid rid body opts = doRequest ac "PATCH" ("/v1/projects/" <> pid <> "/routers/" <> rid <> "/metadata") (Just body) opts
