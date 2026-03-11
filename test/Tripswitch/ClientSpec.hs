module Tripswitch.ClientSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', tryReadTBQueue, writeTVar)
import Control.Exception (Exception (..), SomeException, throwIO, try)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Tripswitch.Client

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

newTestClient :: IO Client
newTestClient = newClient testConfig

testConfig :: ClientConfig
testConfig =
  defaultConfig
    { cfgProjectID = "test-project"
    , cfgLogger = nullLogger
    , cfgSSEDisabled = True
    , cfgFlusherDisabled = True
    , cfgMetaSyncDisabled = True
    }

setBreakerState :: Client -> Text -> BreakerState -> IO ()
setBreakerState client name state =
  atomically $ modifyTVar' (cBreakerStates client) (Map.insert name state)

setBreakersMetadata :: Client -> [BreakerMeta] -> IO ()
setBreakersMetadata client metas =
  atomically $ writeTVar (cBreakersMeta client) (Just metas)

setRoutersMetadata :: Client -> [RouterMeta] -> IO ()
setRoutersMetadata client metas =
  atomically $ writeTVar (cRoutersMeta client) (Just metas)

drainQueue :: Client -> IO [ReportEntry]
drainQueue client = go []
  where
    go acc = do
      mEntry <- atomically $ tryReadTBQueue (cReportQueue client)
      case mEntry of
        Nothing -> pure (reverse acc)
        Just entry -> go (entry : acc)

data TestException = TestException deriving (Show)
instance Exception TestException

data IgnoredException = IgnoredException deriving (Show)
instance Exception IgnoredException

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Client lifecycle" $ do
    it "creates and closes a client" $ do
      client <- newTestClient
      closeClient client

    it "close is idempotent" $ do
      client <- newTestClient
      closeClient client
      closeClient client

    it "withClient ensures cleanup" $ do
      withClient testConfig $ \_ -> pure ()

    it "newClient returns immediately without API key" $ do
      client <- newClient defaultConfig
        { cfgProjectID = "p"
        , cfgLogger = nullLogger
        , cfgSSEDisabled = True
        , cfgFlusherDisabled = True
        , cfgMetaSyncDisabled = True
        }
      closeClient client

  describe "Execute — no breakers" $ do
    it "passes through when no breakers specified" $ do
      client <- newTestClient
      result <- execute client defaultExecConfig (pure (42 :: Int))
      result `shouldBe` Right 42
      closeClient client

  describe "Execute — breaker states" $ do
    it "allows execution when breaker is closed" $ do
      client <- newTestClient
      setBreakerState client "brk" Closed
      result <- execute client defaultExecConfig { ecBreakers = ["brk"] } (pure (1 :: Int))
      result `shouldBe` Right 1
      closeClient client

    it "denies execution when breaker is open" $ do
      client <- newTestClient
      setBreakerState client "brk" Open
      result <- execute client defaultExecConfig { ecBreakers = ["brk"] } (pure (1 :: Int))
      result `shouldBe` Left ErrBreakerOpen
      closeClient client

    it "fails open for unknown breaker" $ do
      client <- newTestClient
      result <- execute client defaultExecConfig { ecBreakers = ["unknown"] } (pure (1 :: Int))
      result `shouldBe` Right 1
      closeClient client

    it "fails open for unrecognized state string" $ do
      client <- newTestClient
      result <- execute client defaultExecConfig { ecBreakers = ["never-seen"] } (pure (1 :: Int))
      result `shouldBe` Right 1
      closeClient client

    it "half-open 0% always denies" $ do
      client <- newTestClient
      setBreakerState client "brk" (HalfOpen 0.0)
      results <- sequence [execute client defaultExecConfig { ecBreakers = ["brk"] } (pure (1 :: Int)) | _ <- [1..20 :: Int]]
      all (== Left ErrBreakerOpen) results `shouldBe` True
      closeClient client

    it "half-open 100% always allows" $ do
      client <- newTestClient
      setBreakerState client "brk" (HalfOpen 1.0)
      results <- sequence [execute client defaultExecConfig { ecBreakers = ["brk"] } (pure (1 :: Int)) | _ <- [1..20 :: Int]]
      all (== Right 1) results `shouldBe` True
      closeClient client

  describe "Execute — multiple breakers" $ do
    it "denies if any breaker is open" $ do
      client <- newTestClient
      setBreakerState client "a" Closed
      setBreakerState client "b" Open
      result <- execute client defaultExecConfig { ecBreakers = ["a", "b"] } (pure (1 :: Int))
      result `shouldBe` Left ErrBreakerOpen
      closeClient client

    it "allows if all breakers are closed" $ do
      client <- newTestClient
      setBreakerState client "a" Closed
      setBreakerState client "b" Closed
      result <- execute client defaultExecConfig { ecBreakers = ["a", "b"] } (pure (1 :: Int))
      result `shouldBe` Right 1
      closeClient client

    it "half-open uses minimum allow rate (statistical)" $ do
      client <- newTestClient
      setBreakerState client "a" (HalfOpen 0.2)
      setBreakerState client "b" (HalfOpen 0.5)
      let n = 10000 :: Int
      results <- sequence [execute client defaultExecConfig { ecBreakers = ["a", "b"] } (pure (1 :: Int)) | _ <- [1..n]]
      let allowed = length [() | Right _ <- results]
          rate = fromIntegral allowed / fromIntegral n :: Double
      rate `shouldSatisfy` (>= 0.15)
      rate `shouldSatisfy` (<= 0.25)
      closeClient client

  describe "Execute — metrics" $ do
    it "emits latency metric" $ do
      client <- newTestClient
      _ <- execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "latency" MetricLatency
        } (pure (42 :: Int))
      entries <- drainQueue client
      length entries `shouldBe` 1
      reMetric (head entries) `shouldBe` "latency"
      reRouterID (head entries) `shouldBe` "rtr-1"
      reOK (head entries) `shouldBe` True
      reValue (head entries) `shouldSatisfy` (>= 0)
      closeClient client

    it "emits multiple metrics" $ do
      client <- newTestClient
      _ <- execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.fromList
            [ ("latency", MetricLatency)
            , ("count", MetricLiteral 1.0)
            , ("amount", MetricLiteral 99.5)
            ]
        } (pure (42 :: Int))
      entries <- drainQueue client
      length entries `shouldBe` 3
      closeClient client

    it "emits no samples without router" $ do
      client <- newTestClient
      _ <- execute client defaultExecConfig
        { ecMetrics = Map.singleton "latency" MetricLatency
        } (pure (42 :: Int))
      entries <- drainQueue client
      length entries `shouldBe` 0
      closeClient client

    it "emits no samples without metrics" $ do
      client <- newTestClient
      _ <- execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        } (pure (42 :: Int))
      entries <- drainQueue client
      length entries `shouldBe` 0
      closeClient client

    it "handles metric closure" $ do
      client <- newTestClient
      _ <- execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "custom" (MetricClosure (pure 42.0))
        } (pure (1 :: Int))
      entries <- drainQueue client
      length entries `shouldBe` 1
      reValue (head entries) `shouldBe` 42.0
      closeClient client

    it "recovers from panicking metric closure" $ do
      client <- newTestClient
      _ <- execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.fromList
            [ ("safe", MetricLiteral 1.0)
            , ("boom", MetricClosure (error "kaboom"))
            ]
        } (pure (1 :: Int))
      entries <- drainQueue client
      length entries `shouldBe` 1
      reMetric (head entries) `shouldBe` "safe"
      closeClient client

    it "metrics without router warns but succeeds" $ do
      client <- newTestClient
      result <- execute client defaultExecConfig
        { ecMetrics = Map.singleton "latency" MetricLatency
        } (pure (42 :: Int))
      result `shouldBe` Right 42
      entries <- drainQueue client
      length entries `shouldBe` 0
      closeClient client

    it "metrics-only with no gating (observability)" $ do
      client <- newTestClient
      result <- execute client defaultExecConfig
        { ecRouterID = "metrics-router"
        , ecMetrics = Map.singleton "latency" MetricLatency
        } (pure ("success" :: String))
      result `shouldBe` Right "success"
      entries <- drainQueue client
      length entries `shouldBe` 1
      reRouterID (head entries) `shouldBe` "metrics-router"
      reMetric (head entries) `shouldBe` "latency"
      closeClient client

    it "gating only, no metrics emitted" $ do
      client <- newTestClient
      setBreakerState client "brk" Closed
      result <- execute client defaultExecConfig
        { ecBreakers = ["brk"]
        } (pure (1 :: Int))
      result `shouldBe` Right 1
      entries <- drainQueue client
      length entries `shouldBe` 0
      closeClient client

    it "router ID appears in samples" $ do
      client <- newTestClient
      _ <- execute client defaultExecConfig
        { ecRouterID = "custom-router"
        , ecMetrics = Map.singleton "m" (MetricLiteral 1.0)
        } (pure (1 :: Int))
      entries <- drainQueue client
      reRouterID (head entries) `shouldBe` "custom-router"
      closeClient client

  describe "Execute — deferred metrics" $ do
    it "emits deferred metrics from result" $ do
      client <- newTestClient
      result <- executeWithDeferred client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "latency" MetricLatency
        }
        (\(_mVal :: Maybe Int) _mExc ->
            pure $ Map.fromList
              [ ("prompt_tokens", 100.0)
              , ("completion_tokens", 200.0)
              , ("total_tokens", 300.0)
              ])
        (pure (42 :: Int))
      result `shouldBe` Right 42
      entries <- drainQueue client
      length entries `shouldBe` 4
      let byMetric = Map.fromList [(reMetric e, reValue e) | e <- entries]
      Map.lookup "prompt_tokens" byMetric `shouldBe` Just 100.0
      Map.lookup "completion_tokens" byMetric `shouldBe` Just 200.0
      Map.lookup "total_tokens" byMetric `shouldBe` Just 300.0
      closeClient client

    it "nil result with deferred metrics emits no deferred samples" $ do
      client <- newTestClient
      _ <- try @SomeException $ executeWithDeferred client defaultExecConfig
        { ecRouterID = "rtr-1"
        }
        (\(_mVal :: Maybe Int) _mExc -> pure Map.empty)
        (throwIO TestException)
      entries <- drainQueue client
      length entries `shouldBe` 0
      closeClient client

    it "recovers from panicking deferred metrics" $ do
      client <- newTestClient
      _ <- executeWithDeferred client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "count" (MetricLiteral 1.0)
        }
        (\(_mVal :: Maybe String) _mExc ->
            error "boom" :: IO (Map.Map Text Double))
        (pure "ok")
      entries <- drainQueue client
      length entries `shouldBe` 1
      reMetric (head entries) `shouldBe` "count"
      closeClient client

  describe "Execute — tags" $ do
    it "merges global and per-call tags" $ do
      client <- newClient testConfig {cfgGlobalTags = Map.fromList [("env", "prod"), ("shared", "global")]}
      _ <- execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "m" (MetricLiteral 1.0)
        , ecTags = Map.fromList [("endpoint", "/api"), ("shared", "local")]
        } (pure (1 :: Int))
      entries <- drainQueue client
      let tags = reTags (head entries)
      Map.lookup "env" tags `shouldBe` Just "prod"
      Map.lookup "endpoint" tags `shouldBe` Just "/api"
      Map.lookup "shared" tags `shouldBe` Just "local"
      closeClient client

    it "multiple tags set all tags" $ do
      client <- newTestClient
      _ <- execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "count" (MetricLiteral 1.0)
        , ecTags = Map.fromList [("endpoint", "/checkout"), ("method", "POST")]
        } (pure ("ok" :: String))
      entries <- drainQueue client
      let tags = reTags (head entries)
      Map.lookup "endpoint" tags `shouldBe` Just "/checkout"
      Map.lookup "method" tags `shouldBe` Just "POST"
      closeClient client

  describe "mergeTags" $ do
    it "nil dynamic returns global" $ do
      mergeTags (Map.fromList [("a", "1")]) Map.empty
        `shouldBe` Map.fromList [("a", "1")]

    it "nil global returns dynamic" $ do
      mergeTags Map.empty (Map.fromList [("x", "10")])
        `shouldBe` Map.fromList [("x", "10")]

    it "dynamic overrides global on conflict" $ do
      let result = mergeTags
            (Map.fromList [("a", "1"), ("b", "2")])
            (Map.fromList [("a", "override"), ("c", "3")])
      Map.lookup "a" result `shouldBe` Just "override"
      Map.lookup "b" result `shouldBe` Just "2"
      Map.lookup "c" result `shouldBe` Just "3"

  describe "Execute — trace ID" $ do
    it "uses explicit trace ID" $ do
      client <- newTestClient
      _ <- execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "m" (MetricLiteral 1.0)
        , ecTraceID = "trace-123"
        } (pure (1 :: Int))
      entries <- drainQueue client
      reTraceID (head entries) `shouldBe` "trace-123"
      closeClient client

    it "uses trace extractor" $ do
      client <- newClient testConfig {cfgTraceExtractor = Just (pure "extracted-456")}
      _ <- execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "m" (MetricLiteral 1.0)
        } (pure (1 :: Int))
      entries <- drainQueue client
      reTraceID (head entries) `shouldBe` "extracted-456"
      closeClient client

    it "explicit trace ID overrides extractor" $ do
      client <- newClient testConfig {cfgTraceExtractor = Just (pure "extracted")}
      _ <- execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "m" (MetricLiteral 1.0)
        , ecTraceID = "explicit"
        } (pure (1 :: Int))
      entries <- drainQueue client
      reTraceID (head entries) `shouldBe` "explicit"
      closeClient client

  describe "Execute — error evaluation" $ do
    it "task error produces ok=false" $ do
      client <- newTestClient
      _ <- try @SomeException $ execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "m" (MetricLiteral 1.0)
        } (throwIO TestException)
      entries <- drainQueue client
      length entries `shouldBe` 1
      reOK (head entries) `shouldBe` False
      closeClient client

    it "ignored error produces ok=true" $ do
      client <- newTestClient
      _ <- try @SomeException $ execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "m" (MetricLiteral 1.0)
        , ecIgnoreErrors = [\e -> show e == show TestException]
        } (throwIO TestException)
      entries <- drainQueue client
      reOK (head entries) `shouldBe` True
      closeClient client

    it "non-ignored error produces ok=false" $ do
      client <- newTestClient
      _ <- try @SomeException $ execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "m" (MetricLiteral 1.0)
        , ecIgnoreErrors = [\e -> show e == show IgnoredException]
        } (throwIO TestException)
      entries <- drainQueue client
      reOK (head entries) `shouldBe` False
      closeClient client

    it "error evaluator takes precedence over ignore list" $ do
      client <- newTestClient
      _ <- try @SomeException $ execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "m" (MetricLiteral 1.0)
        , ecIgnoreErrors = [\e -> show e == show TestException]
        , ecErrorEvaluator = Just (const True)
        } (throwIO TestException)
      entries <- drainQueue client
      reOK (head entries) `shouldBe` False
      closeClient client

    it "error evaluator returning false means ok=true" $ do
      client <- newTestClient
      _ <- try @SomeException $ execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "m" (MetricLiteral 1.0)
        , ecErrorEvaluator = Just (const False)
        } (throwIO TestException)
      entries <- drainQueue client
      reOK (head entries) `shouldBe` True
      closeClient client

    it "nil error is not a failure" $ do
      client <- newTestClient
      _ <- execute client defaultExecConfig
        { ecRouterID = "rtr-1"
        , ecMetrics = Map.singleton "m" (MetricLiteral 1.0)
        } (pure (1 :: Int))
      entries <- drainQueue client
      reOK (head entries) `shouldBe` True
      closeClient client

  describe "Execute — selectors" $ do
    it "conflicting breaker selectors returns error" $ do
      client <- newTestClient
      result <- execute client defaultExecConfig
        { ecBreakers = ["a"]
        , ecBreakerSelector = Just (const ["b"])
        } (pure (1 :: Int))
      result `shouldBe` Left (ErrConflictingOptions "cannot use both ecBreakers and ecBreakerSelector")
      closeClient client

    it "conflicting router selectors returns error" $ do
      client <- newTestClient
      result <- execute client defaultExecConfig
        { ecRouterID = "a"
        , ecRouterSelector = Just (const "b")
        } (pure (1 :: Int))
      result `shouldBe` Left (ErrConflictingOptions "cannot use both ecRouterID and ecRouterSelector")
      closeClient client

    it "breaker selector with empty cache returns error" $ do
      client <- newTestClient
      result <- execute client defaultExecConfig
        { ecBreakerSelector = Just (const ["b"])
        } (pure (1 :: Int))
      result `shouldBe` Left ErrMetadataUnavailable
      closeClient client

    it "router selector with empty cache returns error" $ do
      client <- newTestClient
      result <- execute client defaultExecConfig
        { ecRouterSelector = Just (const "r")
        } (pure (1 :: Int))
      result `shouldBe` Left ErrMetadataUnavailable
      closeClient client

    it "breaker selector selects by metadata (closed = allow)" $ do
      client <- newTestClient
      setBreakersMetadata client
        [ BreakerMeta "b1" "breaker-east" (Map.singleton "region" "us-east-1")
        , BreakerMeta "b2" "breaker-west" (Map.singleton "region" "us-west-2")
        ]
      setBreakerState client "breaker-east" Closed
      setBreakerState client "breaker-west" Open

      result <- execute client defaultExecConfig
        { ecBreakerSelector = Just (\metas ->
            [bmName m | m <- metas, Map.lookup "region" (bmMetadata m) == Just "us-east-1"])
        } (pure ("success" :: String))
      result `shouldBe` Right "success"
      closeClient client

    it "breaker selector selects by metadata (open = deny)" $ do
      client <- newTestClient
      setBreakersMetadata client
        [ BreakerMeta "b1" "breaker-east" (Map.singleton "region" "us-east-1")
        , BreakerMeta "b2" "breaker-west" (Map.singleton "region" "us-west-2")
        ]
      setBreakerState client "breaker-east" Closed
      setBreakerState client "breaker-west" Open

      result <- execute client defaultExecConfig
        { ecBreakerSelector = Just (\metas ->
            [bmName m | m <- metas, Map.lookup "region" (bmMetadata m) == Just "us-west-2"])
        } (pure ("should-not-run" :: String))
      result `shouldBe` Left ErrBreakerOpen
      closeClient client

    it "router selector selects by metadata" $ do
      client <- newTestClient
      setRoutersMetadata client
        [ RouterMeta "r1" "router-prod" (Map.singleton "env" "production")
        , RouterMeta "r2" "router-staging" (Map.singleton "env" "staging")
        ]

      _ <- execute client defaultExecConfig
        { ecRouterSelector = Just (\metas ->
            case [rmID m | m <- metas, Map.lookup "env" (rmMetadata m) == Just "production"] of
              (rid : _) -> rid
              [] -> "")
        , ecMetrics = Map.singleton "count" (MetricLiteral 1.0)
        } (pure ("ok" :: String))
      entries <- drainQueue client
      length entries `shouldBe` 1
      reRouterID (head entries) `shouldBe` "r1"
      closeClient client

    it "breaker selector empty selection = no gating" $ do
      client <- newTestClient
      setBreakersMetadata client [BreakerMeta "b1" "breaker1" Map.empty]

      result <- execute client defaultExecConfig
        { ecBreakerSelector = Just (const [])
        } (pure ("success" :: String))
      result `shouldBe` Right "success"
      closeClient client

    it "router selector empty selection = no samples" $ do
      client <- newTestClient
      setRoutersMetadata client [RouterMeta "r1" "router1" Map.empty]

      _ <- execute client defaultExecConfig
        { ecRouterSelector = Just (const "")
        , ecMetrics = Map.singleton "count" (MetricLiteral 1.0)
        } (pure ("ok" :: String))
      entries <- drainQueue client
      length entries `shouldBe` 0
      closeClient client

    it "breaker selector panic recovers, proceeds without gating" $ do
      client <- newTestClient
      setBreakersMetadata client [BreakerMeta "b1" "breaker1" Map.empty]

      result <- execute client defaultExecConfig
        { ecBreakerSelector = Just (\_ -> error "boom" :: [Text])
        } (pure ("success" :: String))
      result `shouldBe` Right "success"
      closeClient client

    it "router selector panic recovers, no samples emitted" $ do
      client <- newTestClient
      setRoutersMetadata client [RouterMeta "r1" "router1" Map.empty]

      result <- execute client defaultExecConfig
        { ecRouterSelector = Just (\_ -> error "boom" :: Text)
        , ecMetrics = Map.singleton "count" (MetricLiteral 1.0)
        } (pure ("success" :: String))
      result `shouldBe` Right "success"
      entries <- drainQueue client
      length entries `shouldBe` 0
      closeClient client

  describe "Report" $ do
    it "submits a report entry" $ do
      client <- newTestClient
      report client ReportInput
        { riRouterID = "rtr-1"
        , riMetric = "latency"
        , riValue = 42.0
        , riOK = True
        , riTraceID = "trace-1"
        , riTags = Map.fromList [("k", "v")]
        }
      entries <- drainQueue client
      length entries `shouldBe` 1
      reRouterID (head entries) `shouldBe` "rtr-1"
      reMetric (head entries) `shouldBe` "latency"
      reValue (head entries) `shouldBe` 42.0
      reOK (head entries) `shouldBe` True
      reTraceID (head entries) `shouldBe` "trace-1"
      Map.lookup "k" (reTags (head entries)) `shouldBe` Just "v"
      closeClient client

    it "merges global tags in report" $ do
      client <- newClient testConfig {cfgGlobalTags = Map.fromList [("env", "prod")]}
      report client ReportInput
        { riRouterID = "rtr-1"
        , riMetric = "m"
        , riValue = 1.0
        , riOK = True
        , riTraceID = ""
        , riTags = Map.fromList [("local", "yes")]
        }
      entries <- drainQueue client
      let tags = reTags (head entries)
      Map.lookup "env" tags `shouldBe` Just "prod"
      Map.lookup "local" tags `shouldBe` Just "yes"
      closeClient client

    it "skips report without router_id" $ do
      client <- newTestClient
      report client ReportInput
        { riRouterID = ""
        , riMetric = "m"
        , riValue = 1.0
        , riOK = True
        , riTraceID = ""
        , riTags = Map.empty
        }
      entries <- drainQueue client
      length entries `shouldBe` 0
      closeClient client

    it "skips report without metric" $ do
      client <- newTestClient
      report client ReportInput
        { riRouterID = "rtr-1"
        , riMetric = ""
        , riValue = 1.0
        , riOK = True
        , riTraceID = ""
        , riTags = Map.empty
        }
      entries <- drainQueue client
      length entries `shouldBe` 0
      closeClient client

    it "report with no tags works" $ do
      client <- newTestClient
      report client ReportInput
        { riRouterID = "router"
        , riMetric = "count"
        , riValue = 1.0
        , riOK = True
        , riTraceID = ""
        , riTags = Map.empty
        }
      entries <- drainQueue client
      length entries `shouldBe` 1
      reMetric (head entries) `shouldBe` "count"
      closeClient client

  describe "State queries" $ do
    it "getState returns Nothing for unknown" $ do
      client <- newTestClient
      result <- getState client "unknown"
      result `shouldBe` Nothing
      closeClient client

    it "getState returns correct state" $ do
      client <- newTestClient
      setBreakerState client "brk" (HalfOpen 0.5)
      result <- getState client "brk"
      result `shouldBe` Just (BreakerStatus "brk" (HalfOpen 0.5))
      closeClient client

    it "getAllStates returns all states" $ do
      client <- newTestClient
      setBreakerState client "a" Closed
      setBreakerState client "b" Open
      states <- getAllStates client
      length states `shouldBe` 2
      closeClient client

    it "getAllStates returns empty for fresh client" $ do
      client <- newTestClient
      states <- getAllStates client
      length states `shouldBe` 0
      closeClient client

  describe "Stats" $ do
    it "returns initial stats" $ do
      client <- newTestClient
      stats <- getStats client
      ssDroppedSamples stats `shouldBe` 0
      ssSSEConnected stats `shouldBe` False
      ssCachedBreakers stats `shouldBe` 0
      closeClient client

    it "tracks cached breakers count" $ do
      client <- newTestClient
      setBreakerState client "a" Closed
      setBreakerState client "b" Open
      stats <- getStats client
      ssCachedBreakers stats `shouldBe` 2
      closeClient client

  describe "TripSwitchError" $ do
    it "ErrBreakerOpen is catchable as an exception" $ do
      result <- try @TripSwitchError $ throwIO ErrBreakerOpen
      (result :: Either TripSwitchError ()) `shouldBe` Left ErrBreakerOpen

    it "ErrBreakerOpen equality" $ do
      ErrBreakerOpen `shouldBe` ErrBreakerOpen
      ErrBreakerOpen `shouldNotBe` ErrMetadataUnavailable
