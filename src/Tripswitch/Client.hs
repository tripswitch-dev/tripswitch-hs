{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Tripswitch.Client
-- Description : Core Tripswitch SDK client — types, configuration, and runtime API.
--
-- This is the Layer 1 API. Create a 'Client' with 'withClient' or 'newClient',
-- then use 'execute' to run operations guarded by circuit breakers.
module Tripswitch.Client
  ( -- * Client
    Client (..)
  , ClientConfig (..)
  , defaultConfig

    -- * Lifecycle
  , withClient
  , newClient
  , closeClient

    -- * Execute
  , execute
  , execute_
  , executeWithDeferred

    -- * Execute Configuration
  , ExecConfig (..)
  , defaultExecConfig

    -- * Report
  , report
  , ReportInput (..)
  , defaultReportInput

    -- * Breaker State
  , BreakerState (..)
  , BreakerStatus (..)
  , getState
  , getAllStates

    -- * Metrics
  , MetricValue (..)

    -- * Metadata
  , BreakerMeta (..)
  , RouterMeta (..)
  , getBreakersMetadata
  , getRoutersMetadata

    -- * Stats
  , SDKStats (..)
  , getStats

    -- * Errors
  , TripSwitchError (..)

    -- * Logging
  , Logger (..)
  , defaultLogger
  , nullLogger

    -- * Internal (for Client.Internal and tests)
  , ReportEntry (..)
  , BatchPayload (..)
  , SSEBreakerEvent (..)
  , BreakersMetadataResponse (..)
  , RoutersMetadataResponse (..)
  , mergeTags
  , enqueueReport
  , getCurrentTimeMs
  , trySafe
  , parseBreakerState

    -- * Re-exports
  , Text
  ) where

import Control.Concurrent.Async (Async, cancel)
import Control.Concurrent.STM
  ( TBQueue
  , TMVar
  , TVar
  , atomically
  , isFullTBQueue
  , newEmptyTMVarIO
  , newTBQueueIO
  , newTVarIO
  , readTVar
  , readTVarIO
  , tryPutTMVar
  , writeTBQueue
  , writeTVar
  )
import Control.Exception
  ( Exception (..)
  , SomeException
  , bracket
  , catch
  , evaluate
  , throwIO
  , try
  )
import Control.Monad (forM_, unless, void, when)
import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import System.Random (randomRIO)

-- ---------------------------------------------------------------------------
-- Breaker State
-- ---------------------------------------------------------------------------

-- | The state of a circuit breaker as reported by the TripSwitch server.
data BreakerState
  = Closed
  | Open
  | HalfOpen !Double
  deriving stock (Eq, Show)

-- | Parse a wire-format state string into a 'BreakerState'.
parseBreakerState :: Text -> Maybe Double -> Maybe BreakerState
parseBreakerState "closed" _ = Just Closed
parseBreakerState "open" _ = Just Open
parseBreakerState "half_open" mRate = Just (HalfOpen (maybe 0.0 id mRate))
parseBreakerState _ _ = Nothing

-- | Public view of a breaker's status, including its name.
data BreakerStatus = BreakerStatus
  { bsName :: !Text
  , bsState :: !BreakerState
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Errors originating from the TripSwitch SDK itself (not from user tasks).
data TripSwitchError
  = ErrBreakerOpen
  | ErrConflictingOptions !Text
  | ErrMetadataUnavailable
  deriving stock (Eq, Show)

instance Exception TripSwitchError

-- ---------------------------------------------------------------------------
-- Metric Value
-- ---------------------------------------------------------------------------

-- | A metric value to report. Sum type replacing Go's untyped @any@.
data MetricValue
  = MetricLatency
  | MetricLiteral !Double
  | MetricClosure ~(IO Double)

-- ---------------------------------------------------------------------------
-- Wire Types
-- ---------------------------------------------------------------------------

data ReportEntry = ReportEntry
  { reRouterID :: !Text
  , reMetric :: !Text
  , reTsMs :: !Int64
  , reValue :: !Double
  , reOK :: !Bool
  , reTags :: !(Map Text Text)
  , reTraceID :: !Text
  }
  deriving stock (Eq, Show)

instance ToJSON ReportEntry where
  toJSON re =
    object $
      [ "router_id" .= reRouterID re
      , "metric" .= reMetric re
      , "ts_ms" .= reTsMs re
      , "value" .= reValue re
      , "ok" .= reOK re
      ]
        <> ["tags" .= reTags re | not (Map.null (reTags re))]
        <> ["trace_id" .= reTraceID re | not (T.null (reTraceID re))]

newtype BatchPayload = BatchPayload {bpSamples :: [ReportEntry]}

instance ToJSON BatchPayload where
  toJSON bp = object ["samples" .= bpSamples bp]

data SSEBreakerEvent = SSEBreakerEvent
  { sseBreaker :: !Text
  , sseState :: !Text
  , sseAllowRate :: !(Maybe Double)
  }
  deriving stock (Eq, Show)

instance FromJSON SSEBreakerEvent where
  parseJSON = withObject "SSEBreakerEvent" $ \v ->
    SSEBreakerEvent
      <$> v .: "breaker"
      <*> v .: "state"
      <*> v .:? "allow_rate"

-- ---------------------------------------------------------------------------
-- Metadata Types
-- ---------------------------------------------------------------------------

data BreakerMeta = BreakerMeta
  { bmID :: !Text
  , bmName :: !Text
  , bmMetadata :: !(Map Text Text)
  }
  deriving stock (Eq, Show)

instance FromJSON BreakerMeta where
  parseJSON = withObject "BreakerMeta" $ \v ->
    BreakerMeta
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "metadata"

data RouterMeta = RouterMeta
  { rmID :: !Text
  , rmName :: !Text
  , rmMetadata :: !(Map Text Text)
  }
  deriving stock (Eq, Show)

instance FromJSON RouterMeta where
  parseJSON = withObject "RouterMeta" $ \v ->
    RouterMeta
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "metadata"

newtype BreakersMetadataResponse = BreakersMetadataResponse [BreakerMeta]

instance FromJSON BreakersMetadataResponse where
  parseJSON = withObject "BreakersMetadataResponse" $ \v ->
    BreakersMetadataResponse <$> v .: "breakers"

newtype RoutersMetadataResponse = RoutersMetadataResponse [RouterMeta]

instance FromJSON RoutersMetadataResponse where
  parseJSON = withObject "RoutersMetadataResponse" $ \v ->
    RoutersMetadataResponse <$> v .: "routers"

-- ---------------------------------------------------------------------------
-- Status & Stats
-- ---------------------------------------------------------------------------

data SDKStats = SDKStats
  { ssDroppedSamples :: !Word64
  , ssBufferSize :: !Int
  , ssSSEConnected :: !Bool
  , ssSSEReconnects :: !Word64
  , ssLastSuccessfulFlush :: !(Maybe UTCTime)
  , ssLastSSEEvent :: !(Maybe UTCTime)
  , ssFlushFailures :: !Word64
  , ssCachedBreakers :: !Int
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Logging
-- ---------------------------------------------------------------------------

data Logger = Logger
  { logInfo :: !(Text -> IO ())
  , logWarn :: !(Text -> IO ())
  , logError :: !(Text -> IO ())
  }

defaultLogger :: Logger
defaultLogger =
  Logger
    { logInfo = \msg -> putStrLn ("[tripswitch] INFO: " <> T.unpack msg)
    , logWarn = \msg -> putStrLn ("[tripswitch] WARN: " <> T.unpack msg)
    , logError = \msg -> putStrLn ("[tripswitch] ERROR: " <> T.unpack msg)
    }

nullLogger :: Logger
nullLogger = Logger (const (pure ())) (const (pure ())) (const (pure ()))

-- ---------------------------------------------------------------------------
-- Client Configuration
-- ---------------------------------------------------------------------------

data ClientConfig = ClientConfig
  { cfgProjectID :: !Text
  , cfgApiKey :: !Text
  , cfgIngestSecret :: !Text
  , cfgBaseURL :: !Text
  , cfgFailOpen :: !Bool
  , cfgLogger :: !Logger
  , cfgGlobalTags :: !(Map Text Text)
  , cfgOnStateChange :: !(Maybe (Text -> BreakerState -> BreakerState -> IO ()))
  , cfgTraceExtractor :: !(Maybe (IO Text))
  , cfgMetaSyncInterval :: !Int
  , cfgSSEDisabled :: !Bool
  , cfgFlusherDisabled :: !Bool
  , cfgMetaSyncDisabled :: !Bool
  }

defaultConfig :: ClientConfig
defaultConfig =
  ClientConfig
    { cfgProjectID = ""
    , cfgApiKey = ""
    , cfgIngestSecret = ""
    , cfgBaseURL = "https://api.tripswitch.dev"
    , cfgFailOpen = True
    , cfgLogger = defaultLogger
    , cfgGlobalTags = Map.empty
    , cfgOnStateChange = Nothing
    , cfgTraceExtractor = Nothing
    , cfgMetaSyncInterval = 30
    , cfgSSEDisabled = False
    , cfgFlusherDisabled = False
    , cfgMetaSyncDisabled = False
    }

-- ---------------------------------------------------------------------------
-- Execute Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for a single 'execute' call. Use 'defaultExecConfig'
-- and modify with record update syntax:
--
-- @
-- execute client defaultExecConfig
--   { ecBreakers = [\"payment-latency\"]
--   , ecRouterID = \"main-router\"
--   , ecMetrics  = Map.singleton \"latency\" MetricLatency
--   } myAction
-- @
data ExecConfig = ExecConfig
  { ecBreakers :: ![Text]
  , ecBreakerSelector :: !(Maybe ([BreakerMeta] -> [Text]))
  , ecRouterID :: !Text
  , ecRouterSelector :: !(Maybe ([RouterMeta] -> Text))
  , ecMetrics :: !(Map Text MetricValue)
  , ecTags :: !(Map Text Text)
  , ecIgnoreErrors :: ![SomeException -> Bool]
  , ecErrorEvaluator :: !(Maybe (SomeException -> Bool))
  , ecTraceID :: !Text
  }

-- | Default execute configuration — no gating, no metrics, no tags.
defaultExecConfig :: ExecConfig
defaultExecConfig =
  ExecConfig
    { ecBreakers = []
    , ecBreakerSelector = Nothing
    , ecRouterID = ""
    , ecRouterSelector = Nothing
    , ecMetrics = Map.empty
    , ecTags = Map.empty
    , ecIgnoreErrors = []
    , ecErrorEvaluator = Nothing
    , ecTraceID = ""
    }

-- ---------------------------------------------------------------------------
-- Report Input
-- ---------------------------------------------------------------------------

data ReportInput = ReportInput
  { riRouterID :: !Text
  , riMetric :: !Text
  , riValue :: !Double
  , riOK :: !Bool
  , riTraceID :: !Text
  , riTags :: !(Map Text Text)
  }
  deriving stock (Eq, Show)

-- | Default report input with all fields empty/zero.
defaultReportInput :: ReportInput
defaultReportInput =
  ReportInput
    { riRouterID = ""
    , riMetric = ""
    , riValue = 0
    , riOK = True
    , riTraceID = ""
    , riTags = Map.empty
    }

-- ---------------------------------------------------------------------------
-- Client (opaque)
-- ---------------------------------------------------------------------------

data Client = Client
  { cConfig :: !ClientConfig
  , cBreakerStates :: !(TVar (Map Text BreakerState))
  , cReportQueue :: !(TBQueue ReportEntry)
  , cDroppedSamples :: !(IORef Word64)
  , cSSEReady :: !(TMVar ())
  , cClosed :: !(TVar Bool)
  , cSSEThread :: !(TVar (Maybe (Async ())))
  , cFlusherThread :: !(TVar (Maybe (Async ())))
  , cMetaSyncThread :: !(TVar (Maybe (Async ())))
  , cBreakersMeta :: !(TVar (Maybe [BreakerMeta]))
  , cRoutersMeta :: !(TVar (Maybe [RouterMeta]))
  , cBreakersETag :: !(TVar Text)
  , cRoutersETag :: !(TVar Text)
  , cSSEConnected :: !(TVar Bool)
  , cSSEReconnects :: !(IORef Word64)
  , cLastSuccessfulFlush :: !(TVar (Maybe UTCTime))
  , cLastSSEEvent :: !(TVar (Maybe UTCTime))
  , cFlushFailures :: !(IORef Word64)
  }

-- ---------------------------------------------------------------------------
-- Client Lifecycle
-- ---------------------------------------------------------------------------

-- | Create a client with 'bracket' to guarantee cleanup.
--
-- __Note:__ This version does not start background threads (SSE, flusher,
-- metadata sync). For a batteries-included client, use @Tripswitch.'Tripswitch.withClient'@
-- from the "Tripswitch" module instead.
withClient :: ClientConfig -> (Client -> IO a) -> IO a
withClient cfg = bracket (newClient cfg) closeClient

-- | Allocate a new client. Does not start background threads.
--
-- For production use, prefer @Tripswitch.'Tripswitch.newClient'@ from the
-- "Tripswitch" module, which starts SSE, flusher, and metadata sync threads
-- and blocks until the initial SSE sync completes (5 s timeout).
newClient :: ClientConfig -> IO Client
newClient cfg = do
  states <- newTVarIO Map.empty
  queue <- newTBQueueIO 10000
  dropped <- newIORef 0
  sseReady <- newEmptyTMVarIO
  closed <- newTVarIO False
  sseThread <- newTVarIO Nothing
  flusherThread <- newTVarIO Nothing
  metaSyncThread <- newTVarIO Nothing
  bMeta <- newTVarIO Nothing
  rMeta <- newTVarIO Nothing
  bETag <- newTVarIO ""
  rETag <- newTVarIO ""
  sseConn <- newTVarIO False
  sseReconns <- newIORef 0
  lastFlush <- newTVarIO Nothing
  lastSSE <- newTVarIO Nothing
  flushFails <- newIORef 0

  let client =
        Client
          { cConfig = cfg
          , cBreakerStates = states
          , cReportQueue = queue
          , cDroppedSamples = dropped
          , cSSEReady = sseReady
          , cClosed = closed
          , cSSEThread = sseThread
          , cFlusherThread = flusherThread
          , cMetaSyncThread = metaSyncThread
          , cBreakersMeta = bMeta
          , cRoutersMeta = rMeta
          , cBreakersETag = bETag
          , cRoutersETag = rETag
          , cSSEConnected = sseConn
          , cSSEReconnects = sseReconns
          , cLastSuccessfulFlush = lastFlush
          , cLastSSEEvent = lastSSE
          , cFlushFailures = flushFails
          }

  when (T.null (cfgApiKey cfg) || cfgSSEDisabled cfg) $
    void $ atomically $ tryPutTMVar sseReady ()

  pure client

closeClient :: Client -> IO ()
closeClient client = do
  alreadyClosed <- atomically $ do
    wasClosed <- readTVar (cClosed client)
    when (not wasClosed) $ writeTVar (cClosed client) True
    pure wasClosed
  unless alreadyClosed $ do
    cancelThread (cSSEThread client)
    cancelThread (cFlusherThread client)
    cancelThread (cMetaSyncThread client)
  where
    cancelThread tvar = do
      mThread <- readTVarIO tvar
      case mThread of
        Nothing -> pure ()
        Just t -> cancel t

-- ---------------------------------------------------------------------------
-- Execute
-- ---------------------------------------------------------------------------

-- | Execute an operation guarded by circuit breakers.
execute :: Client -> ExecConfig -> IO a -> IO (Either TripSwitchError a)
execute client cfg task = executeInternal client cfg Nothing task

-- | Like 'execute', but throws 'TripSwitchError' on breaker open.
execute_ :: Client -> ExecConfig -> IO a -> IO a
execute_ client cfg task = do
  result <- execute client cfg task
  case result of
    Left err -> throwIO err
    Right val -> pure val

-- | Execute with deferred metrics computed from the result.
executeWithDeferred
  :: Client
  -> ExecConfig
  -> (Maybe a -> Maybe SomeException -> IO (Map Text Double))
  -> IO a
  -> IO (Either TripSwitchError a)
executeWithDeferred client cfg deferredFn task =
  executeInternal client cfg (Just deferredFn) task

-- | Internal execute implementation.
executeInternal
  :: Client
  -> ExecConfig
  -> Maybe (Maybe a -> Maybe SomeException -> IO (Map Text Double))
  -> IO a
  -> IO (Either TripSwitchError a)
executeInternal client cfg mDeferredFn task = do
  -- Step 1: Resolve dynamic selectors
  selectorResult <- resolveSelectors client cfg
  case selectorResult of
    Left err -> pure (Left err)
    Right cfg' -> do
      -- Step 2: Breaker gating
      gateResult <- checkBreakers client cfg'
      case gateResult of
        Left err -> pure (Left err)
        Right () -> do
          -- Step 3: Capture start time
          startTime <- getPOSIXTime
          -- Step 4: Run the task
          taskResult <- try task
          -- Step 5: Compute duration
          endTime <- getPOSIXTime
          let durationMs = realToFrac (endTime - startTime) * 1000 :: Double
          -- Step 6: Determine OK/failure
          let isOK = determineOK cfg' taskResult
          -- Step 7: Resolve trace ID
          traceId <- resolveTraceID client cfg'
          -- Step 8: Emit metrics
          emitMetrics client cfg' mDeferredFn durationMs isOK traceId taskResult
          -- Step 9: Return result
          case taskResult of
            Left exc -> throwIO exc
            Right val -> pure (Right val)

-- ---------------------------------------------------------------------------
-- Report
-- ---------------------------------------------------------------------------

report :: Client -> ReportInput -> IO ()
report client ri = do
  let lg = cfgLogger (cConfig client)
  if T.null (riRouterID ri)
    then logWarn lg "report: missing router_id, skipping"
    else
      if T.null (riMetric ri)
        then logWarn lg "report: missing metric, skipping"
        else do
          now <- getCurrentTimeMs
          let tags = mergeTags (cfgGlobalTags (cConfig client)) (riTags ri)
          let entry =
                ReportEntry
                  { reRouterID = riRouterID ri
                  , reMetric = riMetric ri
                  , reTsMs = now
                  , reValue = riValue ri
                  , reOK = riOK ri
                  , reTags = tags
                  , reTraceID = riTraceID ri
                  }
          enqueueReport client entry

-- ---------------------------------------------------------------------------
-- State Queries
-- ---------------------------------------------------------------------------

getState :: Client -> Text -> IO (Maybe BreakerStatus)
getState client name = do
  states <- readTVarIO (cBreakerStates client)
  pure $ case Map.lookup name states of
    Nothing -> Nothing
    Just st -> Just (BreakerStatus name st)

getAllStates :: Client -> IO [BreakerStatus]
getAllStates client = do
  states <- readTVarIO (cBreakerStates client)
  pure [BreakerStatus k v | (k, v) <- Map.toList states]

getBreakersMetadata :: Client -> IO (Maybe [BreakerMeta])
getBreakersMetadata client = readTVarIO (cBreakersMeta client)

getRoutersMetadata :: Client -> IO (Maybe [RouterMeta])
getRoutersMetadata client = readTVarIO (cRoutersMeta client)

getStats :: Client -> IO SDKStats
getStats client = do
  dropped <- readIORef (cDroppedSamples client)
  sseConn <- readTVarIO (cSSEConnected client)
  sseReconns <- readIORef (cSSEReconnects client)
  lastFlush <- readTVarIO (cLastSuccessfulFlush client)
  lastSSE <- readTVarIO (cLastSSEEvent client)
  flushFails <- readIORef (cFlushFailures client)
  states <- readTVarIO (cBreakerStates client)
  pure
    SDKStats
      { ssDroppedSamples = dropped
      , ssBufferSize = 0
      , ssSSEConnected = sseConn
      , ssSSEReconnects = sseReconns
      , ssLastSuccessfulFlush = lastFlush
      , ssLastSSEEvent = lastSSE
      , ssFlushFailures = flushFails
      , ssCachedBreakers = Map.size states
      }

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

resolveSelectors :: Client -> ExecConfig -> IO (Either TripSwitchError ExecConfig)
resolveSelectors client cfg = do
  cfg1 <- case ecBreakerSelector cfg of
    Nothing -> pure (Right cfg)
    Just sel
      | not (null (ecBreakers cfg)) ->
          pure (Left (ErrConflictingOptions "cannot use both ecBreakers and ecBreakerSelector"))
      | otherwise -> do
          mMeta <- getBreakersMetadata client
          case mMeta of
            Nothing -> pure (Left ErrMetadataUnavailable)
            Just meta -> do
              result <- trySafe $ evaluate (sel meta)
              case result of
                Nothing -> do
                  logWarn (cfgLogger (cConfig client)) "breaker selector threw exception, proceeding without gating"
                  pure (Right cfg {ecBreakers = [], ecBreakerSelector = Nothing})
                Just bs -> pure (Right cfg {ecBreakers = bs, ecBreakerSelector = Nothing})
  case cfg1 of
    Left err -> pure (Left err)
    Right cfg' -> case ecRouterSelector cfg' of
      Nothing -> pure (Right cfg')
      Just sel
        | not (T.null (ecRouterID cfg')) ->
            pure (Left (ErrConflictingOptions "cannot use both ecRouterID and ecRouterSelector"))
        | otherwise -> do
            mMeta <- getRoutersMetadata client
            case mMeta of
              Nothing -> pure (Left ErrMetadataUnavailable)
              Just meta -> do
                result <- trySafe $ evaluate (sel meta)
                case result of
                  Nothing -> do
                    logWarn (cfgLogger (cConfig client)) "router selector threw exception, proceeding without router"
                    pure (Right cfg' {ecRouterID = "", ecRouterSelector = Nothing})
                  Just rid -> pure (Right cfg' {ecRouterID = rid, ecRouterSelector = Nothing})

checkBreakers :: Client -> ExecConfig -> IO (Either TripSwitchError ())
checkBreakers _client cfg
  | null (ecBreakers cfg) = pure (Right ())
checkBreakers client cfg = do
  states <- readTVarIO (cBreakerStates client)
  let check [] minRate = Right minRate
      check (b : bs) minRate =
        case Map.lookup b states of
          Nothing -> check bs minRate
          Just Open -> Left ErrBreakerOpen
          Just Closed -> check bs minRate
          Just (HalfOpen rate) -> check bs (min minRate rate)
  case check (ecBreakers cfg) 1.0 of
    Left err -> pure (Left err)
    Right minRate
      | minRate >= 1.0 -> pure (Right ())
      | otherwise -> do
          roll <- randomRIO (0.0 :: Double, 1.0)
          pure $
            if roll < minRate
              then Right ()
              else Left ErrBreakerOpen

determineOK :: ExecConfig -> Either SomeException a -> Bool
determineOK _ (Right _) = True
determineOK cfg (Left exc) =
  case ecErrorEvaluator cfg of
    Just eval -> not (eval exc)
    Nothing -> any (\check -> check exc) (ecIgnoreErrors cfg)

resolveTraceID :: Client -> ExecConfig -> IO Text
resolveTraceID client cfg
  | not (T.null (ecTraceID cfg)) = pure (ecTraceID cfg)
  | otherwise = case cfgTraceExtractor (cConfig client) of
      Just extractor -> extractor `catch` \(_ :: SomeException) -> pure ""
      Nothing -> pure ""

emitMetrics
  :: Client
  -> ExecConfig
  -> Maybe (Maybe a -> Maybe SomeException -> IO (Map Text Double))
  -> Double
  -> Bool
  -> Text
  -> Either SomeException a
  -> IO ()
emitMetrics client cfg mDeferredFn durationMs isOK traceId taskResult = do
  let lg = cfgLogger (cConfig client)
  when (T.null (ecRouterID cfg) && not (Map.null (ecMetrics cfg))) $
    logWarn lg "metrics specified without router, skipping sample emission"
  unless (T.null (ecRouterID cfg) || Map.null (ecMetrics cfg)) $ do
    now <- getCurrentTimeMs
    let tags = mergeTags (cfgGlobalTags (cConfig client)) (ecTags cfg)
    -- Eager metrics
    forM_ (Map.toList (ecMetrics cfg)) $ \(metricName, metricVal) -> do
      mValue <- resolveMetricValue lg durationMs metricVal
      case mValue of
        Nothing -> pure ()
        Just val ->
          enqueueReport client $
            ReportEntry
              { reRouterID = ecRouterID cfg
              , reMetric = metricName
              , reTsMs = now
              , reValue = val
              , reOK = isOK
              , reTags = tags
              , reTraceID = traceId
              }
    -- Deferred metrics
    case mDeferredFn of
      Nothing -> pure ()
      Just deferredFn -> do
        mDeferred <- case taskResult of
          Right val -> trySafe $ deferredFn (Just val) Nothing
          Left exc -> trySafe $ deferredFn Nothing (Just exc)
        case mDeferred of
          Nothing -> logWarn lg "deferred metrics function threw exception"
          Just deferredMap ->
            forM_ (Map.toList deferredMap) $ \(metricName, val) ->
              unless (T.null metricName) $
                enqueueReport client $
                  ReportEntry
                    { reRouterID = ecRouterID cfg
                    , reMetric = metricName
                    , reTsMs = now
                    , reValue = val
                    , reOK = isOK
                    , reTags = tags
                    , reTraceID = traceId
                    }

resolveMetricValue :: Logger -> Double -> MetricValue -> IO (Maybe Double)
resolveMetricValue _ durationMs MetricLatency = pure (Just durationMs)
resolveMetricValue _ _ (MetricLiteral v) = pure (Just v)
resolveMetricValue lg _ (MetricClosure action) = do
  result <- trySafe action
  case result of
    Nothing -> do
      logWarn lg "metric closure threw exception, skipping"
      pure Nothing
    Just v -> pure (Just v)

-- | Merge global and per-call tags. Per-call tags override global.
mergeTags :: Map Text Text -> Map Text Text -> Map Text Text
mergeTags global perCall = Map.union perCall global

-- | Enqueue a report entry. Non-blocking; drops if queue is full.
enqueueReport :: Client -> ReportEntry -> IO ()
enqueueReport client entry = do
  written <- atomically $ do
    full <- isFullTBQueue (cReportQueue client)
    if full
      then pure False
      else do
        writeTBQueue (cReportQueue client) entry
        pure True
  unless written $
    atomicModifyIORef' (cDroppedSamples client) (\n -> (n + 1, ()))

getCurrentTimeMs :: IO Int64
getCurrentTimeMs = round . (* 1000) <$> getPOSIXTime

trySafe :: IO a -> IO (Maybe a)
trySafe action = do
  result <- try (action >>= evaluate)
  case result of
    Left (_ :: SomeException) -> pure Nothing
    Right val -> pure (Just val)
