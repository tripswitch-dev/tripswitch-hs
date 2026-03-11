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

    -- * Pagination
  , ListParams (..)
  , Pager (..)
  , defaultListParams

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

    -- * Status
  , getProjectStatus
  , getProjectStatusWithConfig

    -- * Metadata
  , updateBreakerMetadata
  , updateRouterMetadata

    -- * With pagination params
  , listProjectsWithParams
  , listBreakersWithParams
  , listRoutersWithParams
  , listNotificationChannelsWithParams
  , listEventsWithParams
  , listProjectKeysWithParams

    -- * With custom config
  , listProjectsWithConfig
  , createProjectWithConfig
  , getProjectWithConfig
  , updateProjectWithConfig
  , deleteProjectWithConfig
  , rotateIngestSecretWithConfig
  , listBreakersWithConfig
  , createBreakerWithConfig
  , getBreakerWithConfig
  , updateBreakerWithConfig
  , deleteBreakerWithConfig
  , syncBreakersWithConfig
  , getBreakerStateWithConfig
  , batchGetBreakerStatesWithConfig
  , listRoutersWithConfig
  , createRouterWithConfig
  , getRouterWithConfig
  , updateRouterWithConfig
  , deleteRouterWithConfig
  , linkBreakerWithConfig
  , unlinkBreakerWithConfig
  , listNotificationChannelsWithConfig
  , createNotificationChannelWithConfig
  , getNotificationChannelWithConfig
  , updateNotificationChannelWithConfig
  , deleteNotificationChannelWithConfig
  , testNotificationChannelWithConfig
  , listEventsWithConfig
  , listProjectKeysWithConfig
  , createProjectKeyWithConfig
  , deleteProjectKeyWithConfig
  , updateBreakerMetadataWithConfig
  , updateRouterMetadataWithConfig
  ) where

import qualified Data.CaseInsensitive as CI
import Control.Exception (SomeException, throwIO, try)
import Data.Aeson (FromJSON (..), Value, eitherDecode, encode)
import Data.Aeson qualified as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
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
  , responseHeaders
  , responseStatus
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Text.Read (readMaybe)

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

doRequest_ :: AdminClient -> Text -> Text -> Maybe Value -> RequestConfig -> IO ()
doRequest_ ac httpMethod path mBody rc = do
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
        Right (Aeson.Object o) ->
          case KM.lookup (Key.fromText "message") o of
            Just (Aeson.String m) -> m
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
      retryAfter = do
        val <- lookup "Retry-After" (responseHeaders resp)
        readMaybe (T.unpack (TE.decodeUtf8 val))
   in APIError kind status msg retryAfter

appendListParams :: Text -> ListParams -> Text
appendListParams path lp =
  let params = []
        <> maybe [] (\c -> ["cursor=" <> c]) (lpCursor lp)
        <> maybe [] (\l -> ["limit=" <> T.pack (show l)]) (lpLimit lp)
   in case params of
        [] -> path
        ps -> path <> "?" <> T.intercalate "&" ps

-- ---------------------------------------------------------------------------
-- Projects
-- ---------------------------------------------------------------------------

-- | List all projects (first page).
listProjects :: AdminClient -> IO (Pager Project)
listProjects ac = listProjectsWithParams ac defaultListParams

-- | List projects with pagination params.
listProjectsWithParams :: AdminClient -> ListParams -> IO (Pager Project)
listProjectsWithParams ac lp = listProjectsWithConfig ac lp defaultRequestConfig

-- | List projects with pagination params and custom request config.
listProjectsWithConfig :: AdminClient -> ListParams -> RequestConfig -> IO (Pager Project)
listProjectsWithConfig ac lp rc = doRequest ac "GET" (appendListParams "/v1/projects" lp) Nothing rc

-- | Create a project.
createProject :: AdminClient -> Value -> IO Project
createProject ac body = createProjectWithConfig ac body defaultRequestConfig

-- | Create a project with custom request config.
createProjectWithConfig :: AdminClient -> Value -> RequestConfig -> IO Project
createProjectWithConfig ac body rc = doRequest ac "POST" "/v1/projects" (Just body) rc

-- | Get a project by ID.
getProject :: AdminClient -> Text -> IO Project
getProject ac pid = getProjectWithConfig ac pid defaultRequestConfig

-- | Get a project by ID with custom request config.
getProjectWithConfig :: AdminClient -> Text -> RequestConfig -> IO Project
getProjectWithConfig ac pid rc = doRequest ac "GET" ("/v1/projects/" <> pid) Nothing rc

-- | Update a project.
updateProject :: AdminClient -> Text -> Value -> IO Project
updateProject ac pid body = updateProjectWithConfig ac pid body defaultRequestConfig

-- | Update a project with custom request config.
updateProjectWithConfig :: AdminClient -> Text -> Value -> RequestConfig -> IO Project
updateProjectWithConfig ac pid body rc = doRequest ac "PATCH" ("/v1/projects/" <> pid) (Just body) rc

-- | Delete a project.
deleteProject :: AdminClient -> Text -> Value -> IO ()
deleteProject ac pid body = deleteProjectWithConfig ac pid body defaultRequestConfig

-- | Delete a project with custom request config.
deleteProjectWithConfig :: AdminClient -> Text -> Value -> RequestConfig -> IO ()
deleteProjectWithConfig ac pid body rc = doRequest_ ac "DELETE" ("/v1/projects/" <> pid) (Just body) rc

-- | Rotate a project's ingest secret.
rotateIngestSecret :: AdminClient -> Text -> IO Value
rotateIngestSecret ac pid = rotateIngestSecretWithConfig ac pid defaultRequestConfig

-- | Rotate a project's ingest secret with custom request config.
rotateIngestSecretWithConfig :: AdminClient -> Text -> RequestConfig -> IO Value
rotateIngestSecretWithConfig ac pid rc = doRequest ac "POST" ("/v1/projects/" <> pid <> "/ingest_secret/rotate") Nothing rc

-- ---------------------------------------------------------------------------
-- Breakers
-- ---------------------------------------------------------------------------

-- | List breakers for a project (first page).
listBreakers :: AdminClient -> Text -> IO (Pager Breaker)
listBreakers ac pid = listBreakersWithParams ac pid defaultListParams

-- | List breakers with pagination params.
listBreakersWithParams :: AdminClient -> Text -> ListParams -> IO (Pager Breaker)
listBreakersWithParams ac pid lp = listBreakersWithConfig ac pid lp defaultRequestConfig

-- | List breakers with pagination params and custom request config.
listBreakersWithConfig :: AdminClient -> Text -> ListParams -> RequestConfig -> IO (Pager Breaker)
listBreakersWithConfig ac pid lp rc = doRequest ac "GET" (appendListParams ("/v1/projects/" <> pid <> "/breakers") lp) Nothing rc

-- | Create a breaker.
createBreaker :: AdminClient -> Text -> Value -> IO Breaker
createBreaker ac pid body = createBreakerWithConfig ac pid body defaultRequestConfig

-- | Create a breaker with custom request config.
createBreakerWithConfig :: AdminClient -> Text -> Value -> RequestConfig -> IO Breaker
createBreakerWithConfig ac pid body rc = doRequest ac "POST" ("/v1/projects/" <> pid <> "/breakers") (Just body) rc

-- | Get a breaker by ID.
getBreaker :: AdminClient -> Text -> Text -> IO Breaker
getBreaker ac pid bid = getBreakerWithConfig ac pid bid defaultRequestConfig

-- | Get a breaker by ID with custom request config.
getBreakerWithConfig :: AdminClient -> Text -> Text -> RequestConfig -> IO Breaker
getBreakerWithConfig ac pid bid rc = doRequest ac "GET" ("/v1/projects/" <> pid <> "/breakers/" <> bid) Nothing rc

-- | Update a breaker.
updateBreaker :: AdminClient -> Text -> Text -> Value -> IO Breaker
updateBreaker ac pid bid body = updateBreakerWithConfig ac pid bid body defaultRequestConfig

-- | Update a breaker with custom request config.
updateBreakerWithConfig :: AdminClient -> Text -> Text -> Value -> RequestConfig -> IO Breaker
updateBreakerWithConfig ac pid bid body rc = doRequest ac "PATCH" ("/v1/projects/" <> pid <> "/breakers/" <> bid) (Just body) rc

-- | Delete a breaker.
deleteBreaker :: AdminClient -> Text -> Text -> IO ()
deleteBreaker ac pid bid = deleteBreakerWithConfig ac pid bid defaultRequestConfig

-- | Delete a breaker with custom request config.
deleteBreakerWithConfig :: AdminClient -> Text -> Text -> RequestConfig -> IO ()
deleteBreakerWithConfig ac pid bid rc = doRequest_ ac "DELETE" ("/v1/projects/" <> pid <> "/breakers/" <> bid) Nothing rc

-- | Sync (bulk replace) breakers.
syncBreakers :: AdminClient -> Text -> Value -> IO [Breaker]
syncBreakers ac pid body = syncBreakersWithConfig ac pid body defaultRequestConfig

-- | Sync (bulk replace) breakers with custom request config.
syncBreakersWithConfig :: AdminClient -> Text -> Value -> RequestConfig -> IO [Breaker]
syncBreakersWithConfig ac pid body rc = doRequest ac "PUT" ("/v1/projects/" <> pid <> "/breakers") (Just body) rc

-- | Get a breaker's state.
getBreakerState :: AdminClient -> Text -> Text -> IO AdminBreakerState
getBreakerState ac pid bid = getBreakerStateWithConfig ac pid bid defaultRequestConfig

-- | Get a breaker's state with custom request config.
getBreakerStateWithConfig :: AdminClient -> Text -> Text -> RequestConfig -> IO AdminBreakerState
getBreakerStateWithConfig ac pid bid rc = doRequest ac "GET" ("/v1/projects/" <> pid <> "/breakers/" <> bid <> "/state") Nothing rc

-- | Batch get breaker states.
batchGetBreakerStates :: AdminClient -> Text -> Value -> IO [AdminBreakerState]
batchGetBreakerStates ac pid body = batchGetBreakerStatesWithConfig ac pid body defaultRequestConfig

-- | Batch get breaker states with custom request config.
batchGetBreakerStatesWithConfig :: AdminClient -> Text -> Value -> RequestConfig -> IO [AdminBreakerState]
batchGetBreakerStatesWithConfig ac pid body rc = doRequest ac "POST" ("/v1/projects/" <> pid <> "/breakers/state:batch") (Just body) rc

-- ---------------------------------------------------------------------------
-- Routers
-- ---------------------------------------------------------------------------

-- | List routers for a project (first page).
listRouters :: AdminClient -> Text -> IO (Pager Router)
listRouters ac pid = listRoutersWithParams ac pid defaultListParams

-- | List routers with pagination params.
listRoutersWithParams :: AdminClient -> Text -> ListParams -> IO (Pager Router)
listRoutersWithParams ac pid lp = listRoutersWithConfig ac pid lp defaultRequestConfig

-- | List routers with pagination params and custom request config.
listRoutersWithConfig :: AdminClient -> Text -> ListParams -> RequestConfig -> IO (Pager Router)
listRoutersWithConfig ac pid lp rc = doRequest ac "GET" (appendListParams ("/v1/projects/" <> pid <> "/routers") lp) Nothing rc

-- | Create a router.
createRouter :: AdminClient -> Text -> Value -> IO Router
createRouter ac pid body = createRouterWithConfig ac pid body defaultRequestConfig

-- | Create a router with custom request config.
createRouterWithConfig :: AdminClient -> Text -> Value -> RequestConfig -> IO Router
createRouterWithConfig ac pid body rc = doRequest ac "POST" ("/v1/projects/" <> pid <> "/routers") (Just body) rc

-- | Get a router by ID.
getRouter :: AdminClient -> Text -> Text -> IO Router
getRouter ac pid rid = getRouterWithConfig ac pid rid defaultRequestConfig

-- | Get a router by ID with custom request config.
getRouterWithConfig :: AdminClient -> Text -> Text -> RequestConfig -> IO Router
getRouterWithConfig ac pid rid rc = doRequest ac "GET" ("/v1/projects/" <> pid <> "/routers/" <> rid) Nothing rc

-- | Update a router.
updateRouter :: AdminClient -> Text -> Text -> Value -> IO Router
updateRouter ac pid rid body = updateRouterWithConfig ac pid rid body defaultRequestConfig

-- | Update a router with custom request config.
updateRouterWithConfig :: AdminClient -> Text -> Text -> Value -> RequestConfig -> IO Router
updateRouterWithConfig ac pid rid body rc = doRequest ac "PATCH" ("/v1/projects/" <> pid <> "/routers/" <> rid) (Just body) rc

-- | Delete a router.
deleteRouter :: AdminClient -> Text -> Text -> IO ()
deleteRouter ac pid rid = deleteRouterWithConfig ac pid rid defaultRequestConfig

-- | Delete a router with custom request config.
deleteRouterWithConfig :: AdminClient -> Text -> Text -> RequestConfig -> IO ()
deleteRouterWithConfig ac pid rid rc = doRequest_ ac "DELETE" ("/v1/projects/" <> pid <> "/routers/" <> rid) Nothing rc

-- | Link a breaker to a router.
linkBreaker :: AdminClient -> Text -> Text -> Value -> IO Value
linkBreaker ac pid rid body = linkBreakerWithConfig ac pid rid body defaultRequestConfig

-- | Link a breaker to a router with custom request config.
linkBreakerWithConfig :: AdminClient -> Text -> Text -> Value -> RequestConfig -> IO Value
linkBreakerWithConfig ac pid rid body rc = doRequest ac "POST" ("/v1/projects/" <> pid <> "/routers/" <> rid <> "/breakers") (Just body) rc

-- | Unlink a breaker from a router.
unlinkBreaker :: AdminClient -> Text -> Text -> Text -> IO ()
unlinkBreaker ac pid rid bid = unlinkBreakerWithConfig ac pid rid bid defaultRequestConfig

-- | Unlink a breaker from a router with custom request config.
unlinkBreakerWithConfig :: AdminClient -> Text -> Text -> Text -> RequestConfig -> IO ()
unlinkBreakerWithConfig ac pid rid bid rc = doRequest_ ac "DELETE" ("/v1/projects/" <> pid <> "/routers/" <> rid <> "/breakers/" <> bid) Nothing rc

-- ---------------------------------------------------------------------------
-- Notification Channels
-- ---------------------------------------------------------------------------

-- | List notification channels (first page).
listNotificationChannels :: AdminClient -> Text -> IO (Pager NotificationChannel)
listNotificationChannels ac pid = listNotificationChannelsWithParams ac pid defaultListParams

-- | List notification channels with pagination params.
listNotificationChannelsWithParams :: AdminClient -> Text -> ListParams -> IO (Pager NotificationChannel)
listNotificationChannelsWithParams ac pid lp = listNotificationChannelsWithConfig ac pid lp defaultRequestConfig

-- | List notification channels with pagination params and custom request config.
listNotificationChannelsWithConfig :: AdminClient -> Text -> ListParams -> RequestConfig -> IO (Pager NotificationChannel)
listNotificationChannelsWithConfig ac pid lp rc = doRequest ac "GET" (appendListParams ("/v1/projects/" <> pid <> "/notification-channels") lp) Nothing rc

-- | Create a notification channel.
createNotificationChannel :: AdminClient -> Text -> Value -> IO NotificationChannel
createNotificationChannel ac pid body = createNotificationChannelWithConfig ac pid body defaultRequestConfig

-- | Create a notification channel with custom request config.
createNotificationChannelWithConfig :: AdminClient -> Text -> Value -> RequestConfig -> IO NotificationChannel
createNotificationChannelWithConfig ac pid body rc = doRequest ac "POST" ("/v1/projects/" <> pid <> "/notification-channels") (Just body) rc

-- | Get a notification channel.
getNotificationChannel :: AdminClient -> Text -> Text -> IO NotificationChannel
getNotificationChannel ac pid cid = getNotificationChannelWithConfig ac pid cid defaultRequestConfig

-- | Get a notification channel with custom request config.
getNotificationChannelWithConfig :: AdminClient -> Text -> Text -> RequestConfig -> IO NotificationChannel
getNotificationChannelWithConfig ac pid cid rc = doRequest ac "GET" ("/v1/projects/" <> pid <> "/notification-channels/" <> cid) Nothing rc

-- | Update a notification channel.
updateNotificationChannel :: AdminClient -> Text -> Text -> Value -> IO NotificationChannel
updateNotificationChannel ac pid cid body = updateNotificationChannelWithConfig ac pid cid body defaultRequestConfig

-- | Update a notification channel with custom request config.
updateNotificationChannelWithConfig :: AdminClient -> Text -> Text -> Value -> RequestConfig -> IO NotificationChannel
updateNotificationChannelWithConfig ac pid cid body rc = doRequest ac "PATCH" ("/v1/projects/" <> pid <> "/notification-channels/" <> cid) (Just body) rc

-- | Delete a notification channel.
deleteNotificationChannel :: AdminClient -> Text -> Text -> IO ()
deleteNotificationChannel ac pid cid = deleteNotificationChannelWithConfig ac pid cid defaultRequestConfig

-- | Delete a notification channel with custom request config.
deleteNotificationChannelWithConfig :: AdminClient -> Text -> Text -> RequestConfig -> IO ()
deleteNotificationChannelWithConfig ac pid cid rc = doRequest_ ac "DELETE" ("/v1/projects/" <> pid <> "/notification-channels/" <> cid) Nothing rc

-- | Test a notification channel.
testNotificationChannel :: AdminClient -> Text -> Text -> IO ()
testNotificationChannel ac pid cid = testNotificationChannelWithConfig ac pid cid defaultRequestConfig

-- | Test a notification channel with custom request config.
testNotificationChannelWithConfig :: AdminClient -> Text -> Text -> RequestConfig -> IO ()
testNotificationChannelWithConfig ac pid cid rc = doRequest_ ac "POST" ("/v1/projects/" <> pid <> "/notification-channels/" <> cid <> "/test") Nothing rc

-- ---------------------------------------------------------------------------
-- Events
-- ---------------------------------------------------------------------------

-- | List events for a project (first page).
listEvents :: AdminClient -> Text -> IO (Pager Event)
listEvents ac pid = listEventsWithParams ac pid defaultListParams

-- | List events with pagination params.
listEventsWithParams :: AdminClient -> Text -> ListParams -> IO (Pager Event)
listEventsWithParams ac pid lp = listEventsWithConfig ac pid lp defaultRequestConfig

-- | List events with pagination params and custom request config.
listEventsWithConfig :: AdminClient -> Text -> ListParams -> RequestConfig -> IO (Pager Event)
listEventsWithConfig ac pid lp rc = doRequest ac "GET" (appendListParams ("/v1/projects/" <> pid <> "/events") lp) Nothing rc

-- ---------------------------------------------------------------------------
-- Project Keys
-- ---------------------------------------------------------------------------

-- | List project keys (first page).
listProjectKeys :: AdminClient -> Text -> IO (Pager ProjectKey)
listProjectKeys ac pid = listProjectKeysWithParams ac pid defaultListParams

-- | List project keys with pagination params.
listProjectKeysWithParams :: AdminClient -> Text -> ListParams -> IO (Pager ProjectKey)
listProjectKeysWithParams ac pid lp = listProjectKeysWithConfig ac pid lp defaultRequestConfig

-- | List project keys with pagination params and custom request config.
listProjectKeysWithConfig :: AdminClient -> Text -> ListParams -> RequestConfig -> IO (Pager ProjectKey)
listProjectKeysWithConfig ac pid lp rc = doRequest ac "GET" (appendListParams ("/v1/projects/" <> pid <> "/keys") lp) Nothing rc

-- | Create a project key.
createProjectKey :: AdminClient -> Text -> Value -> IO ProjectKey
createProjectKey ac pid body = createProjectKeyWithConfig ac pid body defaultRequestConfig

-- | Create a project key with custom request config.
createProjectKeyWithConfig :: AdminClient -> Text -> Value -> RequestConfig -> IO ProjectKey
createProjectKeyWithConfig ac pid body rc = doRequest ac "POST" ("/v1/projects/" <> pid <> "/keys") (Just body) rc

-- | Delete a project key.
deleteProjectKey :: AdminClient -> Text -> Text -> IO ()
deleteProjectKey ac pid kid = deleteProjectKeyWithConfig ac pid kid defaultRequestConfig

-- | Delete a project key with custom request config.
deleteProjectKeyWithConfig :: AdminClient -> Text -> Text -> RequestConfig -> IO ()
deleteProjectKeyWithConfig ac pid kid rc = doRequest_ ac "DELETE" ("/v1/projects/" <> pid <> "/keys/" <> kid) Nothing rc

-- ---------------------------------------------------------------------------
-- Status
-- ---------------------------------------------------------------------------

-- | Get project status.
getProjectStatus :: AdminClient -> Text -> IO Status
getProjectStatus ac pid = getProjectStatusWithConfig ac pid defaultRequestConfig

-- | Get project status with custom request config.
getProjectStatusWithConfig :: AdminClient -> Text -> RequestConfig -> IO Status
getProjectStatusWithConfig ac pid rc = doRequest ac "GET" ("/v1/projects/" <> pid <> "/status") Nothing rc

-- ---------------------------------------------------------------------------
-- Metadata
-- ---------------------------------------------------------------------------

-- | Update breaker metadata.
updateBreakerMetadata :: AdminClient -> Text -> Text -> Value -> IO Value
updateBreakerMetadata ac pid bid body = updateBreakerMetadataWithConfig ac pid bid body defaultRequestConfig

-- | Update breaker metadata with custom request config.
updateBreakerMetadataWithConfig :: AdminClient -> Text -> Text -> Value -> RequestConfig -> IO Value
updateBreakerMetadataWithConfig ac pid bid body rc = doRequest ac "PATCH" ("/v1/projects/" <> pid <> "/breakers/" <> bid <> "/metadata") (Just body) rc

-- | Update router metadata.
updateRouterMetadata :: AdminClient -> Text -> Text -> Value -> IO Value
updateRouterMetadata ac pid rid body = updateRouterMetadataWithConfig ac pid rid body defaultRequestConfig

-- | Update router metadata with custom request config.
updateRouterMetadataWithConfig :: AdminClient -> Text -> Text -> Value -> RequestConfig -> IO Value
updateRouterMetadataWithConfig ac pid rid body rc = doRequest ac "PATCH" ("/v1/projects/" <> pid <> "/routers/" <> rid <> "/metadata") (Just body) rc
