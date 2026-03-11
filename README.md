# Tripswitch Haskell SDK

[![Hackage](https://img.shields.io/hackage/v/tripswitch.svg)](https://hackage.haskell.org/package/tripswitch)
[![Haskell-CI](https://img.shields.io/badge/GHC-9.6+-blue)](https://www.haskell.org)

Official Haskell SDK for [Tripswitch](https://tripswitch.dev) — a circuit breaker management service.

This SDK conforms to the [Tripswitch SDK Contract v0.2](https://tripswitch.dev/docs/sdk-contract).

```haskell
import Tripswitch
import Tripswitch.Effect (CircuitBreaker, cbExecute)

fetchData :: (CircuitBreaker :> es, IOE :> es) => Eff es (Either TripSwitchError String)
fetchData =
  cbExecute defaultExecConfig
    { ecBreakers = ["data-service"]
    , ecRouterID = "main-router"
    , ecMetrics  = Map.singleton "latency" MetricLatency
    }
    (pure "data from service")
```

## Features

- **Sum type breaker state** — `Closed | Open | HalfOpen !Double`, not strings
- **Real-time state sync** via Server-Sent Events (SSE)
- **Automatic sample reporting** with buffered, gzip-compressed, HMAC-signed uploads
- **STM concurrency** — `TVar` for shared state, `TBQueue` for report buffering
- **Fail-open by default** — your app stays available even if Tripswitch is unreachable
- **Effect system** — circuit breaking as an [effectful](https://hackage.haskell.org/package/effectful-core) effect with a pure test interpreter
- **GADT state machine** — type-level proof that a breaker allows traffic
- **Graceful shutdown** via `bracket` with automatic sample flushing

## Installation

Add to your `.cabal` file:

```cabal
build-depends:
  , tripswitch >= 0.1
```

**Requires GHC 9.6+**

## Architecture

The SDK is organized in three layers of increasing type sophistication. Use whichever level suits your codebase — they compose freely.

### Layer 2: Effect System (`Tripswitch.Effect`) — Recommended

Circuit breaking as an [effectful](https://hackage.haskell.org/package/effectful-core) effect. Declare circuit breaker dependencies in your type signatures, then swap interpreters for production vs testing:

```haskell
fetchData :: (CircuitBreaker :> es, IOE :> es) => Eff es (Either TripSwitchError String)
fetchData =
  cbExecute defaultExecConfig
    { ecBreakers = ["payment-service"]
    , ecRouterID = "main-router"
    , ecMetrics  = Map.singleton "latency" MetricLatency
    }
    (pure "data from service")
```

Comes with a **pure test interpreter** — test breaker logic without network or IO:

```haskell
stateRef <- newIORef defaultPureState
  { psBreakerStates = Map.singleton "payment-service" Open }
result <- runEff $ runCircuitBreakerPure stateRef fetchData
-- result is Left ErrBreakerOpen
```

### Layer 1: Core API (`Tripswitch.Client`)

Clean ADTs and IO-based API. `BreakerState` is a sum type — pattern match to handle each case:

```haskell
data BreakerState = Closed | Open | HalfOpen !Double
```

`execute` returns `Either TripSwitchError a`, making the breaker-open case explicit in the type:

```haskell
execute :: Client -> ExecConfig -> IO a -> IO (Either TripSwitchError a)
```

### Layer 3: Typed State Machine (`Tripswitch.Typed`)

GADTs indexed by breaker phase. The compiler ensures you only execute through verified breakers:

```haskell
result <- checkBreaker client "payment-latency"
case result of
  Left blocked  -> handleBlocked blocked     -- TBreaker 'POpen
  Right witness -> executeTyped client witness defaultExecConfig myAction  -- AllowWitness
```

`checkBreaker` returns `Either (TBreaker 'POpen) AllowWitness`. The `AllowWitness` can only be constructed by `checkBreaker` when the breaker is closed or a half-open probabilistic check passes — making "execute through an unchecked breaker" a compile-time error.

## Authentication

Tripswitch uses a two-tier authentication model:

### Runtime Credentials (SDK)

For SDK initialization, from **Project Settings > SDK Keys**:

| Credential | Prefix | Purpose |
|------------|--------|---------|
| **Project Key** | `eb_pk_` | SSE connection and state reads |
| **Ingest Secret** | `ik_` | HMAC-signed sample ingestion |

### Admin Credentials (Management API)

For management tasks, from **Organization Settings > Admin Keys**:

| Credential | Prefix | Purpose |
|------------|--------|---------|
| **Admin Key** | `eb_admin_` | Organization-scoped management operations |

Admin keys are used with the [Admin Client](#admin-client) — not for runtime SDK usage.

## Quick Start: IO

```haskell
import qualified Data.Map.Strict as Map
import Tripswitch

main :: IO ()
main = do
  let cfg = defaultConfig
        { cfgProjectID    = "proj_123"
        , cfgApiKey       = "eb_pk_..."
        , cfgIngestSecret = "ik_..."
        }

  withClient cfg $ \client -> do
    result <- execute client defaultExecConfig
      { ecBreakers = ["external-api"]
      , ecRouterID = "my-router-id"
      , ecMetrics  = Map.singleton "latency" MetricLatency
      }
      (httpGet "https://api.example.com/data")

    case result of
      Left ErrBreakerOpen -> putStrLn "Circuit open, using fallback"
      Left err            -> print err
      Right response      -> processResponse response
```

## Quick Start: Effect System

```haskell
import qualified Data.Map.Strict as Map
import Effectful (Eff, IOE, runEff, (:>))
import Tripswitch
import Tripswitch.Effect (CircuitBreaker, cbExecute)
import Tripswitch.Effect.Interpreters (runCircuitBreakerIO)

fetchData :: (CircuitBreaker :> es, IOE :> es) => Eff es (Either TripSwitchError String)
fetchData =
  cbExecute defaultExecConfig
    { ecBreakers = ["data-service"]
    , ecRouterID = "main-router"
    , ecMetrics  = Map.singleton "latency" MetricLatency
    }
    (pure "data from service")

main :: IO ()
main = do
  let cfg = defaultConfig
        { cfgProjectID = "proj_123"
        , cfgApiKey    = "eb_pk_..."
        }

  withClient cfg $ \client -> do
    result <- runEff $ runCircuitBreakerIO client fetchData
    case result of
      Left ErrBreakerOpen -> putStrLn "Breaker open!"
      Right val           -> putStrLn $ "Got: " <> val
```

## Configuration

Configure the client with record update syntax on `defaultConfig`:

```haskell
let cfg = defaultConfig
      { cfgProjectID      = "proj_123"
      , cfgApiKey         = "eb_pk_..."
      , cfgIngestSecret   = "ik_..."
      , cfgGlobalTags     = Map.fromList [("env", "prod")]
      , cfgTraceExtractor = Just getTraceFromContext
      , cfgOnStateChange  = Just $ \name old new ->
          putStrLn $ name <> ": " <> show old <> " -> " <> show new
      }
```

### Client Config Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `cfgProjectID` | `Text` | `""` | Project ID |
| `cfgApiKey` | `Text` | `""` | Project key (`eb_pk_`) for SSE auth |
| `cfgIngestSecret` | `ByteString` | `""` | Ingest secret (`ik_`) for HMAC signing |
| `cfgBaseURL` | `Text` | `"https://api.tripswitch.dev"` | API endpoint |
| `cfgFailOpen` | `Bool` | `True` | Allow traffic when Tripswitch is unreachable |
| `cfgLogger` | `Logger` | `defaultLogger` | Custom logger |
| `cfgGlobalTags` | `Map Text Text` | `mempty` | Tags applied to all samples |
| `cfgTraceExtractor` | `Maybe (IO Text)` | `Nothing` | Extract trace ID for each sample |
| `cfgOnStateChange` | `Maybe (Text -> BreakerState -> BreakerState -> IO ())` | `Nothing` | Callback on breaker state transitions |
| `cfgMetaSyncInterval` | `Maybe Int` | `Just 30` | Metadata sync interval (seconds). `Nothing` to disable |

### Execute Configuration

Configure each `execute` call with record update syntax on `defaultExecConfig`:

```haskell
execute client defaultExecConfig
  { ecBreakers = ["payment-latency"]
  , ecRouterID = "main-router"
  , ecMetrics  = Map.fromList [("latency", MetricLatency), ("bytes", MetricLiteral 4096)]
  , ecTags     = Map.singleton "endpoint" "/api/charge"
  }
  myTask
```

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `ecBreakers` | `[Text]` | `[]` | Breaker names to check (any open = `ErrBreakerOpen`) |
| `ecBreakerSelector` | `Maybe ([BreakerMeta] -> [Text])` | `Nothing` | Dynamically select breakers from cached metadata |
| `ecRouterID` | `Text` | `""` | Router ID for sample routing |
| `ecRouterSelector` | `Maybe ([RouterMeta] -> Text)` | `Nothing` | Dynamically select a router from cached metadata |
| `ecMetrics` | `Map Text MetricValue` | `mempty` | Metrics to report |
| `ecTags` | `Map Text Text` | `mempty` | Diagnostic tags (merged with global; per-call wins on conflict) |
| `ecIgnoreErrors` | `[SomeException -> Bool]` | `[]` | Errors that should not count as failures |
| `ecErrorEvaluator` | `Maybe (SomeException -> Bool)` | `Nothing` | Custom failure predicate (takes precedence over `ecIgnoreErrors`) |
| `ecTraceID` | `Text` | `""` | Explicit trace ID (takes precedence over `cfgTraceExtractor`) |

**Constraints:**
- `ecBreakers` and `ecBreakerSelector` are mutually exclusive (`ErrConflictingOptions`)
- `ecRouterID` and `ecRouterSelector` are mutually exclusive (`ErrConflictingOptions`)

## API Reference

### Client Lifecycle

```haskell
withClient  :: ClientConfig -> (Client -> IO a) -> IO a  -- bracket (recommended)
newClient   :: ClientConfig -> IO Client                   -- explicit create
closeClient :: Client -> IO ()                             -- idempotent
```

`withClient` is the recommended entry point. It uses `bracket` to guarantee cleanup — background threads are cancelled and buffered samples are flushed even if an exception is thrown.

### Execute

```haskell
execute  :: Client -> ExecConfig -> IO a -> IO (Either TripSwitchError a)
execute_ :: Client -> ExecConfig -> IO a -> IO a  -- throws on breaker open

executeWithDeferred
  :: Client -> ExecConfig
  -> (a -> Maybe SomeException -> IO (Map Text Double))
  -> IO a -> IO (Either TripSwitchError a)
```

Runs a task end-to-end: checks breaker state, executes the task, captures metrics, and enqueues samples — all in one call.

- `Left` = the SDK blocked execution (breaker open, conflicting options, metadata unavailable)
- `Right` = the task ran (task exceptions propagate normally in IO after metrics are captured)

### Report

```haskell
report :: Client -> ReportInput -> IO ()
```

Submit a sample independently of `execute`. For async workflows, result-derived metrics, or fire-and-forget reporting:

```haskell
report client ReportInput
  { riRouterID = "llm-router"
  , riMetric   = "total_tokens"
  , riValue    = fromIntegral tokenCount
  , riOK       = True
  , riTags     = Map.singleton "model" "claude"
  , riTraceID  = ""
  }
```

### Breaker State Inspection

```haskell
getState     :: Client -> Text -> IO (Maybe BreakerStatus)
getAllStates  :: Client -> IO (Map Text BreakerStatus)
```

Read the local breaker cache for debugging, logging, and health checks. Returns copies — safe to hold without affecting internal state. For gating traffic, use `execute` with `ecBreakers`.

### Stats

```haskell
getStats :: Client -> IO SDKStats
```

Returns a snapshot of SDK health metrics: dropped samples, buffer size, SSE status, flush history.

## Errors

```haskell
data TripSwitchError
  = ErrBreakerOpen
  | ErrConflictingOptions !Text
  | ErrMetadataUnavailable
```

| Error | Cause |
|-------|-------|
| `ErrBreakerOpen` | A specified breaker is open or request was throttled in half-open state |
| `ErrConflictingOptions` | Mutually exclusive options used (e.g., `ecBreakers` + `ecBreakerSelector`) |
| `ErrMetadataUnavailable` | Dynamic selector used but metadata cache hasn't been populated yet |

Pattern match on the `Either` return from `execute` — the compiler ensures you handle the error case.

## Metric Values

```haskell
data MetricValue
  = MetricLatency                 -- auto-computed task duration (ms)
  | MetricLiteral !Double         -- static numeric value
  | MetricClosure ~(IO Double)    -- dynamic value computed after task completes
```

```haskell
execute client defaultExecConfig
  { ecMetrics = Map.fromList
      [ ("latency",        MetricLatency)
      , ("response_bytes", MetricLiteral 4096)
      , ("memory_mb",      MetricClosure getMemoryMB)
      ]
  , ecRouterID = "my-router"
  }
  myTask
```

### Deferred Metrics

Use `executeWithDeferred` to extract metrics from the task's return value — useful when the interesting values are in the response:

```haskell
result <- executeWithDeferred client defaultExecConfig
  { ecBreakers = ["anthropic-spend"]
  , ecRouterID = "llm-router"
  , ecMetrics  = Map.singleton "latency" MetricLatency
  }
  (\response _mExc -> case response of
      Nothing  -> pure Map.empty
      Just res -> pure $ Map.fromList
        [ ("prompt_tokens",     fromIntegral $ promptTokens res)
        , ("completion_tokens", fromIntegral $ completionTokens res)
        ]
  )
  (callAnthropic req)
```

Deferred metrics are resolved after the task completes and merged with eager metrics. If the function throws, it is caught and a warning is logged — eager metrics are still emitted.

### Error Classification

Every sample includes an `ok` field. Evaluation order:

1. **`ecErrorEvaluator`** — takes precedence. Return `True` if the error is a failure.
2. **`ecIgnoreErrors`** — if the task exception matches any of these, it is not counted as a failure.
3. **Default** — any exception is a failure; no exception is success.

```haskell
-- Only count specific exceptions as failures
execute client defaultExecConfig
  { ecErrorEvaluator = Just $ \ex ->
      case fromException ex of
        Just (HttpError status _) -> status >= 500
        Nothing                   -> True
  } myTask

-- Ignore expected errors
execute client defaultExecConfig
  { ecIgnoreErrors = [\ex -> case fromException ex of
      Just NotFoundError -> True
      _                  -> False]
  } myTask
```

## Dynamic Selection

Use `ecBreakerSelector` and `ecRouterSelector` to choose breakers or routers at runtime based on cached metadata:

```haskell
-- Gate on breakers matching metadata
execute client defaultExecConfig
  { ecBreakerSelector = Just $ \breakers ->
      [bmName b | b <- breakers, Map.lookup "region" (bmMetadata b) == Just "us-east-1"]
  } myTask

-- Route to a router matching metadata
execute client defaultExecConfig
  { ecRouterSelector = Just $ \routers ->
      case [rmID r | r <- routers, Map.lookup "env" (rmMetadata r) == Just "production"] of
        (rid:_) -> rid
        []      -> ""
  , ecMetrics = Map.singleton "latency" MetricLatency
  } myTask
```

Access the metadata cache directly:

```haskell
getBreakersMetadata :: Client -> IO [BreakerMeta]
getRoutersMetadata  :: Client -> IO [RouterMeta]
```

## Circuit Breaker States

| State | Type | Behavior |
|-------|------|----------|
| Closed | `Closed` | All requests allowed, results reported |
| Open | `Open` | All requests rejected with `ErrBreakerOpen` |
| Half-open | `HalfOpen !Double` | Requests probabilistically allowed based on allow rate |

When multiple breakers are specified, any open breaker blocks execution. For half-open breakers, the minimum allow rate across all specified breakers is used for the probabilistic check.

## How It Works

1. **State Sync** — The client maintains a local `TVar` cache of breaker states, updated in real-time via SSE
2. **Execute Check** — Each `execute` call reads the local cache in a single STM transaction (no network call)
3. **Sample Reporting** — Results are buffered in a `TBQueue` and batched (500 samples or 15s, whichever comes first), gzip-compressed, and HMAC-signed
4. **Graceful Degradation** — If Tripswitch is unreachable, the client fails open by default

## Admin Client

The `Tripswitch.Admin` module provides a client for management and automation tasks. This is separate from the runtime SDK and uses organization-scoped admin keys.

```haskell
import Tripswitch.Admin
import Tripswitch.Admin.Types
import Tripswitch.Admin.Errors

main :: IO ()
main = do
  let cfg = defaultAdminConfig
        { acAdminKey = "eb_admin_..."
        }
  ac <- newAdminClient cfg

  -- List all projects
  projects <- listProjects ac defaultRequestConfig

  -- Create a project
  project <- createProject ac
    (object ["name" .= ("prod-payments" :: Text)]) defaultRequestConfig

  -- Create a breaker
  breaker <- createBreaker ac (projID project)
    (object
      [ "name"      .= ("api-latency" :: Text)
      , "metric"    .= ("latency_ms" :: Text)
      , "kind"      .= ("p95" :: Text)
      , "op"        .= ("gt" :: Text)
      , "threshold" .= (500 :: Int)
      ]) defaultRequestConfig
```

### Request Configuration

Configure individual admin requests with `defaultRequestConfig`:

```haskell
createProject ac body defaultRequestConfig
  { rcIdempotencyKey = Just "unique-key-123"
  , rcRequestID      = Just "req-456"
  , rcExtraHeaders   = Map.singleton "X-Custom" "value"
  }
```

### Error Handling

Admin API errors are thrown as `APIError` exceptions with a typed error kind:

```haskell
data AdminErrorKind
  = ErrNotFound | ErrUnauthorized | ErrForbidden
  | ErrConflict | ErrRateLimited  | ErrValidation
  | ErrServerFault | ErrTransport

-- Predicate functions for catching specific errors
isNotFound, isUnauthorized, isForbidden :: APIError -> Bool
isConflict, isRateLimited, isValidation :: APIError -> Bool
isServerFault, isTransport              :: APIError -> Bool
```

```haskell
result <- try @APIError $ getProject ac "nonexistent" defaultRequestConfig
case result of
  Left err | isNotFound err -> putStrLn "Project not found"
  Left err                  -> throwIO err
  Right project             -> print (projName project)
```

### Available Operations

| Resource | Operations |
|----------|------------|
| **Projects** | `listProjects`, `createProject`, `getProject`, `updateProject`, `deleteProject`, `rotateIngestSecret` |
| **Breakers** | `listBreakers`, `createBreaker`, `getBreaker`, `updateBreaker`, `deleteBreaker`, `syncBreakers`, `getBreakerState`, `batchGetBreakerStates` |
| **Routers** | `listRouters`, `createRouter`, `getRouter`, `updateRouter`, `deleteRouter`, `linkBreaker`, `unlinkBreaker` |
| **Notifications** | `listNotificationChannels`, `createNotificationChannel`, `getNotificationChannel`, `updateNotificationChannel`, `deleteNotificationChannel`, `testNotificationChannel` |
| **Events** | `listEvents` |
| **Project Keys** | `listProjectKeys`, `createProjectKey`, `deleteProjectKey` |

## Examples

See the [`examples/`](examples/) directory:

- **[Basic.hs](examples/Basic.hs)** — IO-based usage with `execute` and `withClient`
- **[EffectSystem.hs](examples/EffectSystem.hs)** — Circuit breaking as an effectful effect
- **[PureTesting.hs](examples/PureTesting.hs)** — Pure test interpreter demo (no IO, no network)
- **[TypedStateMachine.hs](examples/TypedStateMachine.hs)** — GADT state machine with type-level proofs

## Contributing

Contributions are welcome! Please open an issue or submit a pull request.

## License

[Apache License 2.0](LICENSE)
