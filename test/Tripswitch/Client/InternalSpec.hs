module Tripswitch.Client.InternalSpec (spec) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Test.Hspec

import Tripswitch.Client
  ( BatchPayload (..)
  , BreakerState (..)
  , ReportEntry (..)
  , SSEBreakerEvent (..)
  , parseBreakerState
  )
import Tripswitch.Client.Internal
  ( compressPayload
  , contractVersion
  , flusherBatchSize
  , flusherTickSeconds
  , hmacSign
  , ingestRetryBackoffs
  , metaSyncDefaultSeconds
  , parseSSEData
  , reportQueueCapacity
  )

spec :: Spec
spec = do
  -- =======================================================================
  -- SSE parsing
  -- =======================================================================
  describe "SSE parsing" $ do
    it "parses a closed breaker event" $ do
      let input = "{\"breaker\": \"test\", \"state\": \"closed\"}"
      case parseSSEData input of
        Left err -> expectationFailure err
        Right evt -> do
          sseBreaker evt `shouldBe` "test"
          sseState evt `shouldBe` "closed"
          sseAllowRate evt `shouldBe` Nothing

    it "parses an open breaker event" $ do
      let input = "{\"breaker\": \"payment\", \"state\": \"open\", \"allow_rate\": null}"
      case parseSSEData input of
        Left err -> expectationFailure err
        Right evt -> do
          sseBreaker evt `shouldBe` "payment"
          sseState evt `shouldBe` "open"
          sseAllowRate evt `shouldBe` Nothing

    it "parses a half_open breaker event with allow_rate" $ do
      let input = "{\"breaker\": \"test\", \"state\": \"half_open\", \"allow_rate\": 0.25}"
      case parseSSEData input of
        Left err -> expectationFailure err
        Right evt -> do
          sseState evt `shouldBe` "half_open"
          sseAllowRate evt `shouldBe` Just 0.25

    it "parses a half_open event with null allow_rate" $ do
      let input = "{\"breaker\": \"test\", \"state\": \"half_open\", \"allow_rate\": null}"
      case parseSSEData input of
        Left err -> expectationFailure err
        Right evt ->
          sseAllowRate evt `shouldBe` Nothing

    it "rejects malformed JSON" $ do
      let input = "{not json"
      case parseSSEData input of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected parse error"

  -- =======================================================================
  -- parseBreakerState
  -- =======================================================================
  describe "parseBreakerState" $ do
    it "parses closed" $ do
      parseBreakerState "closed" Nothing `shouldBe` Just Closed

    it "parses open" $ do
      parseBreakerState "open" Nothing `shouldBe` Just Open

    it "parses half_open with rate" $ do
      parseBreakerState "half_open" (Just 0.5) `shouldBe` Just (HalfOpen 0.5)

    it "parses half_open with null rate as 0.0" $ do
      parseBreakerState "half_open" Nothing `shouldBe` Just (HalfOpen 0.0)

    it "returns Nothing for unknown state" $ do
      parseBreakerState "bogus" Nothing `shouldBe` Nothing

    it "returns Nothing for empty state" $ do
      parseBreakerState "" Nothing `shouldBe` Nothing

  -- =======================================================================
  -- HMAC signing
  -- =======================================================================
  describe "HMAC signing" $ do
    it "produces a v1= prefixed signature" $ do
      let secret = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
          sig = hmacSign secret "1234567890" "body"
      BS.isPrefixOf "v1=" sig `shouldBe` True

    it "produces a deterministic signature" $ do
      let secret = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
          sig1 = hmacSign secret "12345" "payload"
          sig2 = hmacSign secret "12345" "payload"
      sig1 `shouldBe` sig2

    it "different timestamps produce different signatures" $ do
      let secret = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
          sig1 = hmacSign secret "11111" "body"
          sig2 = hmacSign secret "22222" "body"
      sig1 `shouldNotBe` sig2

    it "different bodies produce different signatures" $ do
      let secret = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
          sig1 = hmacSign secret "12345" "body1"
          sig2 = hmacSign secret "12345" "body2"
      sig1 `shouldNotBe` sig2

  -- =======================================================================
  -- Compression
  -- =======================================================================
  describe "Compression" $ do
    it "compresses and produces non-empty output" $ do
      let compressed = compressPayload "{\"samples\":[]}"
      (LBS.length compressed > 0) `shouldBe` True

    it "compressed output differs from input" $ do
      let input = "{\"samples\":[{\"router_id\":\"r1\",\"metric\":\"m\",\"ts_ms\":0,\"value\":1.0,\"ok\":true}]}"
          compressed = compressPayload input
      compressed `shouldNotBe` input

  -- =======================================================================
  -- Payload format (JSON serialization)
  -- =======================================================================
  describe "Payload format" $ do
    it "BatchPayload serializes correctly" $ do
      let entry = ReportEntry
            { reRouterID = "router-123"
            , reMetric = "error_rate"
            , reTsMs = 1705312200000
            , reValue = 1.0
            , reOK = True
            , reTags = Map.fromList [("tier", "premium")]
            , reTraceID = "abc123"
            }
          payload = BatchPayload [entry]
          json = Aeson.encode payload
      -- Verify it can be decoded
      case Aeson.eitherDecode json of
        Left err -> expectationFailure $ "JSON roundtrip failed: " <> err
        Right (Aeson.Object _) -> pure ()
        Right _ -> expectationFailure "expected JSON object"

    it "ReportEntry omits empty tags and trace_id" $ do
      let entry = ReportEntry
            { reRouterID = "r1"
            , reMetric = "m"
            , reTsMs = 0
            , reValue = 1.0
            , reOK = True
            , reTags = Map.empty
            , reTraceID = ""
            }
          json = Aeson.encode entry
          jsonStr = LBS.toStrict json
      -- Should not contain "tags" or "trace_id" keys
      BS.isInfixOf "\"tags\"" jsonStr `shouldBe` False
      BS.isInfixOf "\"trace_id\"" jsonStr `shouldBe` False

    it "ReportEntry includes tags and trace_id when present" $ do
      let entry = ReportEntry
            { reRouterID = "r1"
            , reMetric = "m"
            , reTsMs = 0
            , reValue = 1.0
            , reOK = True
            , reTags = Map.singleton "k" "v"
            , reTraceID = "t1"
            }
          json = Aeson.encode entry
          jsonStr = LBS.toStrict json
      BS.isInfixOf "\"tags\"" jsonStr `shouldBe` True
      BS.isInfixOf "\"trace_id\"" jsonStr `shouldBe` True

    it "ReportEntry fields are correct in JSON" $ do
      let entry = ReportEntry
            { reRouterID = "router-123"
            , reMetric = "error_rate"
            , reTsMs = 1705312200000
            , reValue = 1.0
            , reOK = True
            , reTags = Map.fromList [("tier", "premium")]
            , reTraceID = "abc123"
            }
          json = Aeson.encode entry
          jsonStr = LBS.toStrict json
      -- Verify key fields are present
      BS.isInfixOf "\"router_id\":\"router-123\"" jsonStr `shouldBe` True
      BS.isInfixOf "\"metric\":\"error_rate\"" jsonStr `shouldBe` True
      BS.isInfixOf "\"ok\":true" jsonStr `shouldBe` True
      BS.isInfixOf "\"trace_id\":\"abc123\"" jsonStr `shouldBe` True

  -- =======================================================================
  -- Constants
  -- =======================================================================
  describe "Constants" $ do
    it "contract version is 0.2" $ do
      contractVersion `shouldBe` "0.2"

    it "report queue capacity is 10000" $ do
      reportQueueCapacity `shouldBe` 10000

    it "flusher batch size is 500" $ do
      flusherBatchSize `shouldBe` 500

    it "flusher tick is 15 seconds" $ do
      flusherTickSeconds `shouldBe` 15

    it "metadata sync default is 30 seconds" $ do
      metaSyncDefaultSeconds `shouldBe` 30

    it "ingest retry backoffs are 100ms, 400ms, 1s" $ do
      ingestRetryBackoffs `shouldBe` [100000, 400000, 1000000]
