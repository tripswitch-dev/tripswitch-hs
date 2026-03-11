{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Tripswitch.Client.Internal
-- Description : SSE listener, flusher, ingest, HMAC signing, metadata sync.
--
-- Internal module exposing background thread implementations.
-- Not intended for direct use — prefer the public API in "Tripswitch.Client".
module Tripswitch.Client.Internal
  ( -- * Background threads
    startSSEListener
  , startFlusher
  , startMetadataSync

    -- * SSE parsing
  , parseSSEData

    -- * Ingest
  , sendBatch
  , compressPayload
  , hmacSign

    -- * Constants
  , contractVersion
  , reportQueueCapacity
  , flusherBatchSize
  , flusherTickSeconds
  , metaSyncDefaultSeconds
  , ingestRetryBackoffs
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , readTVar
  , readTVarIO
  , tryPutTMVar
  , tryReadTBQueue
  , writeTVar
  )
import Control.Exception (SomeException, catch, try)
import Control.Monad (forM_, void, when)
import qualified Codec.Compression.GZip as GZip
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC (..), hmac)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import Data.Aeson (FromJSON, eitherDecodeStrict, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef')
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (..)
  , Response (..)
  , httpLbs
  , parseRequest
  , responseStatus
  )
import Network.HTTP.Types.Status (statusCode)

import Tripswitch.Client
  ( BatchPayload (..)
  , BreakerMeta
  , BreakerState (..)
  , BreakersMetadataResponse (..)
  , Client (..)
  , ClientConfig (..)
  , Logger (..)
  , ReportEntry
  , RouterMeta
  , RoutersMetadataResponse (..)
  , SSEBreakerEvent (..)
  , parseBreakerState
  )

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Contract version for the ingest API.
contractVersion :: Text
contractVersion = "0.2"

-- | Report queue capacity.
reportQueueCapacity :: Int
reportQueueCapacity = 10000

-- | Maximum batch size for flushing.
flusherBatchSize :: Int
flusherBatchSize = 500

-- | Flusher tick interval in seconds.
flusherTickSeconds :: Int
flusherTickSeconds = 15

-- | Default metadata sync interval in seconds.
metaSyncDefaultSeconds :: Int
metaSyncDefaultSeconds = 30

-- | Retry backoffs in microseconds for ingest.
ingestRetryBackoffs :: [Int]
ingestRetryBackoffs = [100000, 400000, 1000000]

-- ---------------------------------------------------------------------------
-- SSE Listener
-- ---------------------------------------------------------------------------

-- | Start the SSE listener background thread.
startSSEListener :: Client -> Manager -> IO (Async ())
startSSEListener client mgr = async $ sseLoop 0
  where
    cfg = cConfig client
    sseLoop retryCount = do
      let url =
            T.unpack (cfgBaseURL cfg)
              <> "/v1/projects/"
              <> T.unpack (cfgProjectID cfg)
              <> "/breakers/state:stream"
      result <- try $ do
        req <- parseRequest url
        let req' =
              req
                { requestHeaders =
                    [ ("Authorization", "Bearer " <> TE.encodeUtf8 (cfgApiKey cfg))
                    , ("Accept", "text/event-stream")
                    , ("Cache-Control", "no-cache")
                    ]
                }
        resp <- httpLbs req' mgr
        parseAndApplySSE client (LBS.toStrict (responseBody resp))
      case result of
        Left (exc :: SomeException) -> do
          atomicModifyIORef' (cSSEReconnects client) (\n -> (n + 1, ()))
          atomically $ writeTVar (cSSEConnected client) False
          logWarn (cfgLogger cfg) $ "SSE connection error: " <> T.pack (show exc)
          let delay = min 30000000 (1000000 * (2 ^ min retryCount (4 :: Int)))
          threadDelay delay
          sseLoop (retryCount + 1)
        Right () -> sseLoop 0

-- | Parse SSE data lines and apply state updates.
parseAndApplySSE :: Client -> ByteString -> IO ()
parseAndApplySSE client rawData = do
  let dataLines = [BS.drop 6 l | l <- BS8.lines rawData, "data: " `BS.isPrefixOf` l]
  forM_ dataLines $ \dataLine ->
    case eitherDecodeStrict dataLine of
      Left err ->
        logWarn (cfgLogger (cConfig client)) $ "SSE parse error: " <> T.pack err
      Right evt -> applySSEEvent client evt

-- | Parse a raw SSE data line.
parseSSEData :: ByteString -> Either String SSEBreakerEvent
parseSSEData = eitherDecodeStrict

-- | Apply a parsed SSE event to the client state.
applySSEEvent :: Client -> SSEBreakerEvent -> IO ()
applySSEEvent client evt = do
  let lg = cfgLogger (cConfig client)
      name = sseBreaker evt
  case parseBreakerState (sseState evt) (sseAllowRate evt) of
    Nothing -> logWarn lg $ "unknown breaker state: " <> sseState evt
    Just newState -> do
      when (sseState evt == "half_open" && sseAllowRate evt == Nothing) $
        logWarn lg $ "half_open breaker " <> name <> " with null allow_rate, defaulting to 0.0"

      mOldState <- atomically $ do
        states <- readTVar (cBreakerStates client)
        let oldState = Map.lookup name states
        modifyTVar' (cBreakerStates client) (Map.insert name newState)
        void $ tryPutTMVar (cSSEReady client) ()
        pure oldState

      atomically $ writeTVar (cSSEConnected client) True
      now <- getCurrentTime
      atomically $ writeTVar (cLastSSEEvent client) (Just now)

      case (mOldState, cfgOnStateChange (cConfig client)) of
        (Just oldState, Just callback)
          | oldState /= newState ->
              callback name oldState newState
                `catch` \(_ :: SomeException) -> pure ()
        _ -> pure ()

-- ---------------------------------------------------------------------------
-- Flusher
-- ---------------------------------------------------------------------------

-- | Start the flusher background thread.
startFlusher :: Client -> Manager -> IO (Async ())
startFlusher client mgr = async $ flusherLoop []
  where
    flusherLoop batch = do
      (newBatch, shouldFlush) <- drainQueue batch
      if shouldFlush
        then do
          flushBatch newBatch
          flusherLoop []
        else do
          threadDelay (flusherTickSeconds * 1000000)
          if null newBatch
            then flusherLoop []
            else do
              flushBatch newBatch
              flusherLoop []

    drainQueue batch = do
      let remaining = flusherBatchSize - length batch
      entries <- drainN remaining
      let newBatch = batch <> entries
      pure (newBatch, length newBatch >= flusherBatchSize)

    drainN 0 = pure []
    drainN n = do
      mEntry <- atomically $ tryReadTBQueue (cReportQueue client)
      case mEntry of
        Nothing -> pure []
        Just entry -> do
          rest <- drainN (n - 1)
          pure (entry : rest)

    flushBatch [] = pure ()
    flushBatch batch = do
      result <- try $ sendBatch client mgr batch
      case result of
        Left (_ :: SomeException) -> do
          atomicModifyIORef' (cFlushFailures client) (\n -> (n + 1, ()))
          let dropped = length batch
          atomicModifyIORef' (cDroppedSamples client) (\n -> (n + fromIntegral dropped, ()))
        Right _ -> pure ()

-- | Send a batch of report entries to the ingest endpoint.
sendBatch :: Client -> Manager -> [ReportEntry] -> IO ()
sendBatch client mgr batch = do
  let cfg = cConfig client
      payload = BatchPayload batch
      jsonBytes = encode payload
      compressed = compressPayload jsonBytes
  tsMs <- getCurrentTimeMs'
  let tsStr = BS8.pack (show tsMs)
      url =
        T.unpack (cfgBaseURL cfg)
          <> "/v1/projects/"
          <> T.unpack (cfgProjectID cfg)
          <> "/ingest"
  req <- parseRequest url
  let mSig =
        if T.null (cfgIngestSecret cfg)
          then Nothing
          else Just (hmacSign (TE.encodeUtf8 (cfgIngestSecret cfg)) tsStr (LBS.toStrict compressed))
      headers =
        [ ("Content-Type", "application/json")
        , ("Content-Encoding", "gzip")
        , ("X-EB-Timestamp", tsStr)
        ]
          <> maybe [] (\sig -> [("X-EB-Signature", sig)]) mSig
      req' =
        req
          { method = "POST"
          , requestHeaders = headers
          , requestBody = RequestBodyLBS compressed
          }
  sendWithRetry client mgr req' ingestRetryBackoffs

sendWithRetry :: Client -> Manager -> Request -> [Int] -> IO ()
sendWithRetry client mgr req backoffs = go (0 :: Int) backoffs
  where
    go _attempt [] = do
      atomicModifyIORef' (cFlushFailures client) (\n -> (n + 1, ()))
      logError (cfgLogger (cConfig client)) "ingest: all retries exhausted"
    go attempt (delay : rest) = do
      result <- try $ httpLbs req mgr
      case result of
        Right resp
          | statusCode (responseStatus resp) >= 200
              && statusCode (responseStatus resp) < 300 -> do
              now <- getCurrentTime
              atomically $ writeTVar (cLastSuccessfulFlush client) (Just now)
        Right resp -> do
          logWarn (cfgLogger (cConfig client)) $
            "ingest: HTTP " <> T.pack (show (statusCode (responseStatus resp)))
          when (attempt < 3) $ do
            threadDelay delay
            go (attempt + 1) rest
        Left (_ :: SomeException) -> do
          when (attempt < 3) $ do
            threadDelay delay
            go (attempt + 1) rest

-- | Compress a lazy bytestring with gzip.
compressPayload :: LBS.ByteString -> LBS.ByteString
compressPayload = GZip.compress

-- | Compute HMAC-SHA256 signature for ingest.
hmacSign :: ByteString -> ByteString -> ByteString -> ByteString
hmacSign hexSecret timestamp body =
  let secretBytes = hexDecode hexSecret
      message = timestamp <> "." <> body
      sig = hmac secretBytes message :: HMAC SHA256
      hexSig = BAE.convertToBase BAE.Base16 (BA.convert sig :: ByteString)
   in "v1=" <> hexSig

hexDecode :: ByteString -> ByteString
hexDecode hex =
  case BAE.convertFromBase BAE.Base16 hex of
    Left _ -> BS.empty
    Right bs -> bs

getCurrentTimeMs' :: IO Int64
getCurrentTimeMs' = round . (* 1000) <$> getPOSIXTime

-- ---------------------------------------------------------------------------
-- Metadata Sync
-- ---------------------------------------------------------------------------

-- | Start the metadata sync background thread.
startMetadataSync :: Client -> Manager -> IO (Async ())
startMetadataSync client mgr = async $ do
  _ <- refreshMetadata client mgr
  metaLoop
  where
    metaLoop = do
      let interval = cfgMetaSyncInterval (cConfig client) * 1000000
      threadDelay interval
      shouldStop <- refreshMetadata client mgr
      if shouldStop
        then pure ()
        else metaLoop

refreshMetadata :: Client -> Manager -> IO Bool
refreshMetadata client mgr = do
  let cfg = cConfig client
  bStop <- fetchMeta client mgr
    (T.unpack (cfgBaseURL cfg) <> "/v1/projects/" <> T.unpack (cfgProjectID cfg) <> "/breakers/metadata")
    (cBreakersETag client)
    (\(BreakersMetadataResponse bs) -> atomically $ writeTVar (cBreakersMeta client) (Just bs))
  rStop <- fetchMeta client mgr
    (T.unpack (cfgBaseURL cfg) <> "/v1/projects/" <> T.unpack (cfgProjectID cfg) <> "/routers/metadata")
    (cRoutersETag client)
    (\(RoutersMetadataResponse rs) -> atomically $ writeTVar (cRoutersMeta client) (Just rs))
  pure (bStop || rStop)

fetchMeta
  :: (FromJSON a)
  => Client
  -> Manager
  -> String
  -> TVar Text
  -> (a -> IO ())
  -> IO Bool
fetchMeta client mgr url etagVar applyResult = do
  let cfg = cConfig client
      lg = cfgLogger cfg
  etag <- readTVarIO etagVar
  result <- try $ do
    req <- parseRequest url
    let headers =
          [("Authorization", "Bearer " <> TE.encodeUtf8 (cfgApiKey cfg))]
            <> [("If-None-Match", TE.encodeUtf8 etag) | not (T.null etag)]
        req' = req {requestHeaders = headers}
    httpLbs req' mgr
  case result of
    Left (exc :: SomeException) -> do
      logWarn lg $ "metadata fetch error: " <> T.pack (show exc)
      pure False
    Right resp -> do
      let status = statusCode (responseStatus resp)
      if status == 304
        then pure False
        else if status == 401 || status == 403
          then do
            logError lg "metadata: auth failure, stopping sync"
            pure True
          else if status >= 200 && status < 300
            then do
              case eitherDecodeStrict (LBS.toStrict (responseBody resp)) of
                Left err -> do
                  logWarn lg $ "metadata parse error: " <> T.pack err
                  pure False
                Right val -> do
                  applyResult val
                  let newETag = maybe "" TE.decodeUtf8 $
                        lookup "ETag" (responseHeaders resp)
                  atomically $ writeTVar etagVar newETag
                  pure False
            else do
              logWarn lg $ "metadata: HTTP " <> T.pack (show status)
              pure False
