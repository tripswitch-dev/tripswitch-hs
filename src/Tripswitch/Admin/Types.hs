{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Tripswitch.Admin.Types
-- Description : Admin API domain types and enums.
module Tripswitch.Admin.Types
  ( -- * Project
    Project (..)

    -- * Breaker
  , Breaker (..)
  , BreakerKind (..)
  , BreakerOp (..)
  , HalfOpenPolicy (..)

    -- * Router
  , Router (..)
  , RouterMode (..)

    -- * Breaker State
  , AdminBreakerState (..)

    -- * Notification
  , NotificationChannel (..)
  , NotificationChannelType (..)
  , NotificationEventType (..)

    -- * Event
  , Event (..)

    -- * Project Key
  , ProjectKey (..)

    -- * Status
  , Status (..)

    -- * List Responses
  , ListProjectsResponse (..)
  , ListBreakersResponse (..)
  , ListRoutersResponse (..)
  , ListEventsResponse (..)
  , ListChannelsResponse (..)
  , ListKeysResponse (..)
  , BatchBreakerStatesResponse (..)

    -- * Pagination
  , ListParams (..)
  , defaultListParams
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (..)
  , withObject
  , withText
  , (.:)
  , (.:?)
  , (.!=)
  )
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Enums
-- ---------------------------------------------------------------------------

-- | Breaker kind — the aggregation method for the breaker.
data BreakerKind
  = ErrorRate
  | Avg
  | P95
  | Max
  | Min
  | Sum
  | StdDev
  | Count
  | Percentile
  | ConsecutiveFailures
  | Delta
  deriving stock (Eq, Show)

instance FromJSON BreakerKind where
  parseJSON = withText "BreakerKind" $ \case
    "error_rate" -> pure ErrorRate
    "avg" -> pure Avg
    "p95" -> pure P95
    "max" -> pure Max
    "min" -> pure Min
    "sum" -> pure Sum
    "stddev" -> pure StdDev
    "count" -> pure Count
    "percentile" -> pure Percentile
    "consecutive_failures" -> pure ConsecutiveFailures
    "delta" -> pure Delta
    other -> fail $ "unknown BreakerKind: " <> T.unpack other

instance ToJSON BreakerKind where
  toJSON ErrorRate = String "error_rate"
  toJSON Avg = String "avg"
  toJSON P95 = String "p95"
  toJSON Max = String "max"
  toJSON Min = String "min"
  toJSON Sum = String "sum"
  toJSON StdDev = String "stddev"
  toJSON Count = String "count"
  toJSON Percentile = String "percentile"
  toJSON ConsecutiveFailures = String "consecutive_failures"
  toJSON Delta = String "delta"

-- | Breaker comparison operator.
data BreakerOp = OpGT | OpLT | OpGTE | OpLTE
  deriving stock (Eq, Show)

instance FromJSON BreakerOp where
  parseJSON = withText "BreakerOp" $ \case
    "gt" -> pure OpGT
    "lt" -> pure OpLT
    "gte" -> pure OpGTE
    "lte" -> pure OpLTE
    other -> fail $ "unknown BreakerOp: " <> T.unpack other

instance ToJSON BreakerOp where
  toJSON OpGT = String "gt"
  toJSON OpLT = String "lt"
  toJSON OpGTE = String "gte"
  toJSON OpLTE = String "lte"

-- | Half-open indeterminate policy (server-side concept).
data HalfOpenPolicy = Optimistic | Conservative | Pessimistic
  deriving stock (Eq, Show)

instance FromJSON HalfOpenPolicy where
  parseJSON = withText "HalfOpenPolicy" $ \case
    "optimistic" -> pure Optimistic
    "conservative" -> pure Conservative
    "pessimistic" -> pure Pessimistic
    other -> fail $ "unknown HalfOpenPolicy: " <> T.unpack other

instance ToJSON HalfOpenPolicy where
  toJSON Optimistic = String "optimistic"
  toJSON Conservative = String "conservative"
  toJSON Pessimistic = String "pessimistic"

-- | Router mode.
data RouterMode = Static | Canary | Weighted
  deriving stock (Eq, Show)

instance FromJSON RouterMode where
  parseJSON = withText "RouterMode" $ \case
    "static" -> pure Static
    "canary" -> pure Canary
    "weighted" -> pure Weighted
    other -> fail $ "unknown RouterMode: " <> T.unpack other

instance ToJSON RouterMode where
  toJSON Static = String "static"
  toJSON Canary = String "canary"
  toJSON Weighted = String "weighted"

-- | Notification channel type.
data NotificationChannelType = Slack | PagerDuty | Email | Webhook
  deriving stock (Eq, Show)

instance FromJSON NotificationChannelType where
  parseJSON = withText "NotificationChannelType" $ \case
    "slack" -> pure Slack
    "pagerduty" -> pure PagerDuty
    "email" -> pure Email
    "webhook" -> pure Webhook
    other -> fail $ "unknown NotificationChannelType: " <> T.unpack other

instance ToJSON NotificationChannelType where
  toJSON Slack = String "slack"
  toJSON PagerDuty = String "pagerduty"
  toJSON Email = String "email"
  toJSON Webhook = String "webhook"

-- | Notification event type.
data NotificationEventType = Trip | Recover
  deriving stock (Eq, Show)

instance FromJSON NotificationEventType where
  parseJSON = withText "NotificationEventType" $ \case
    "trip" -> pure Trip
    "recover" -> pure Recover
    other -> fail $ "unknown NotificationEventType: " <> T.unpack other

instance ToJSON NotificationEventType where
  toJSON Trip = String "trip"
  toJSON Recover = String "recover"

-- ---------------------------------------------------------------------------
-- Domain Types
-- ---------------------------------------------------------------------------

-- | A TripSwitch project.
data Project = Project
  { projID :: !Text
  , projName :: !Text
  , projSlackWebhookURL :: !(Maybe Text)
  , projTraceIDURLTemplate :: !(Maybe Text)
  , projEnableSignedIngest :: !Bool
  }
  deriving stock (Eq, Show)

instance FromJSON Project where
  parseJSON = withObject "Project" $ \v ->
    Project
      <$> v .: "project_id"
      <*> v .:? "name" .!= ""
      <*> v .:? "slack_webhook_url"
      <*> v .:? "trace_id_url_template"
      <*> v .:? "enable_signed_ingest" .!= False

-- | A circuit breaker definition.
data Breaker = Breaker
  { brkID :: !Text
  , brkRouterID :: !(Maybe Text)
  , brkName :: !Text
  , brkMetric :: !Text
  , brkKind :: !BreakerKind
  , brkOp :: !BreakerOp
  , brkThreshold :: !Double
  , brkWindowMs :: !Int64
  , brkMinCount :: !Int
  , brkMinStateDurationMs :: !Int64
  , brkCooldownMs :: !Int64
  , brkEvalIntervalMs :: !Int64
  , brkHalfOpenConfirmationMs :: !(Maybe Int64)
  , brkHalfOpenBackoffEnabled :: !Bool
  , brkHalfOpenBackoffCapMs :: !(Maybe Int64)
  , brkHalfOpenIndeterminatePolicy :: !(Maybe HalfOpenPolicy)
  , brkRecoveryWindowMs :: !(Maybe Int64)
  , brkRecoveryAllowRateRampSteps :: !(Maybe Int)
  , brkActions :: !(Maybe Value)
  , brkKindParams :: !(Maybe Value)
  , brkMetadata :: !(Map Text Text)
  }
  deriving stock (Eq, Show)

instance FromJSON Breaker where
  parseJSON = withObject "Breaker" $ \v ->
    Breaker
      <$> v .: "id"
      <*> v .:? "router_id"
      <*> v .: "name"
      <*> v .: "metric"
      <*> v .: "kind"
      <*> v .: "op"
      <*> v .: "threshold"
      <*> v .: "window_ms"
      <*> v .: "min_count"
      <*> v .: "min_state_duration_ms"
      <*> v .: "cooldown_ms"
      <*> v .: "eval_interval_ms"
      <*> v .:? "half_open_confirmation_ms"
      <*> v .:? "half_open_backoff_enabled" .!= False
      <*> v .:? "half_open_backoff_cap_ms"
      <*> v .:? "half_open_indeterminate_policy"
      <*> v .:? "recovery_window_ms"
      <*> v .:? "recovery_allow_rate_ramp_steps"
      <*> v .:? "actions"
      <*> v .:? "kind_params"
      <*> v .:? "metadata" .!= mempty

-- | A router.
data Router = Router
  { rtrID :: !Text
  , rtrName :: !Text
  , rtrMode :: !RouterMode
  , rtrEnabled :: !Bool
  , rtrBreakerCount :: !Int
  , rtrBreakers :: !(Maybe [Breaker])
  , rtrInsertedAt :: !(Maybe Text)
  , rtrCreatedBy :: !(Maybe Text)
  , rtrMetadata :: !(Map Text Text)
  }
  deriving stock (Eq, Show)

instance FromJSON Router where
  parseJSON = withObject "Router" $ \v ->
    Router
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "mode"
      <*> v .: "enabled"
      <*> v .:? "breaker_count" .!= 0
      <*> v .:? "breakers"
      <*> v .:? "inserted_at"
      <*> v .:? "created_by"
      <*> v .:? "metadata" .!= mempty

-- | Breaker state from the admin API.
data AdminBreakerState = AdminBreakerState
  { absBreakerID :: !Text
  , absName :: !(Maybe Text)
  , absState :: !Text
  , absStateSinceMs :: !(Maybe Int64)
  , absLastEvalMs :: !(Maybe Int64)
  , absNextEvalMs :: !(Maybe Int64)
  , absIndeterminate :: !(Maybe Bool)
  }
  deriving stock (Eq, Show)

instance FromJSON AdminBreakerState where
  parseJSON = withObject "AdminBreakerState" $ \v ->
    AdminBreakerState
      <$> v .: "breaker_id"
      <*> v .:? "name"
      <*> v .: "state"
      <*> v .:? "state_since_ms"
      <*> v .:? "last_eval_ms"
      <*> v .:? "next_eval_ms"
      <*> v .:? "indeterminate"

-- | A notification channel.
data NotificationChannel = NotificationChannel
  { ncID :: !Text
  , ncChannel :: !NotificationChannelType
  , ncConfig :: !(Map Text Value)
  , ncEvents :: ![NotificationEventType]
  , ncEnabled :: !Bool
  , ncCreatedAt :: !(Maybe Text)
  , ncUpdatedAt :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

instance FromJSON NotificationChannel where
  parseJSON = withObject "NotificationChannel" $ \v ->
    NotificationChannel
      <$> v .: "id"
      <*> v .: "channel"
      <*> v .:? "config" .!= mempty
      <*> v .:? "events" .!= []
      <*> v .:? "enabled" .!= True
      <*> v .:? "created_at"
      <*> v .:? "updated_at"

-- | A breaker state change event.
data Event = Event
  { evBreakerID :: !Text
  , evName :: !(Maybe Text)
  , evTsMs :: !Int64
  , evTransitionSeq :: !Int
  , evPrev :: !Text
  , evCurr :: !Text
  , evAgg :: !(Maybe Value)
  , evRule :: !(Maybe Value)
  }
  deriving stock (Eq, Show)

instance FromJSON Event where
  parseJSON = withObject "Event" $ \v ->
    Event
      <$> v .: "breaker_id"
      <*> v .:? "name"
      <*> v .: "ts_ms"
      <*> v .: "transition_seq"
      <*> v .: "prev"
      <*> v .: "curr"
      <*> v .:? "agg"
      <*> v .:? "rule"

-- | A project API key.
data ProjectKey = ProjectKey
  { pkID :: !Text
  , pkName :: !Text
  , pkKeyPrefix :: !Text
  , pkLastUsedAt :: !(Maybe Text)
  , pkInsertedAt :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

instance FromJSON ProjectKey where
  parseJSON = withObject "ProjectKey" $ \v ->
    ProjectKey
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "key_prefix"
      <*> v .:? "last_used_at"
      <*> v .:? "inserted_at"

-- ---------------------------------------------------------------------------
-- Status
-- ---------------------------------------------------------------------------

-- | Project status summary.
data Status = Status
  { stOpenCount :: !Int
  , stClosedCount :: !Int
  , stLastEvalMs :: !Int64
  }
  deriving stock (Eq, Show)

instance FromJSON Status where
  parseJSON = withObject "Status" $ \v ->
    Status
      <$> v .: "open_count"
      <*> v .: "closed_count"
      <*> v .: "last_eval_ms"

-- ---------------------------------------------------------------------------
-- List Response Types
-- ---------------------------------------------------------------------------

-- | Response from listing projects.
data ListProjectsResponse = ListProjectsResponse
  { lprProjects :: ![Project]
  , lprCount :: !Int
  }
  deriving stock (Eq, Show)

instance FromJSON ListProjectsResponse where
  parseJSON = withObject "ListProjectsResponse" $ \v ->
    ListProjectsResponse
      <$> v .: "projects"
      <*> v .:? "count" .!= 0

-- | Response from listing breakers.
data ListBreakersResponse = ListBreakersResponse
  { lbrBreakers :: ![Breaker]
  , lbrCount :: !Int
  , lbrHash :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

instance FromJSON ListBreakersResponse where
  parseJSON = withObject "ListBreakersResponse" $ \v ->
    ListBreakersResponse
      <$> v .: "breakers"
      <*> v .:? "count" .!= 0
      <*> v .:? "hash"

-- | Response from listing routers.
data ListRoutersResponse = ListRoutersResponse
  { lrrRouters :: ![Router]
  }
  deriving stock (Eq, Show)

instance FromJSON ListRoutersResponse where
  parseJSON = withObject "ListRoutersResponse" $ \v ->
    ListRoutersResponse
      <$> v .: "routers"

-- | Response from listing events.
data ListEventsResponse = ListEventsResponse
  { lerEvents :: ![Event]
  , lerReturned :: !Int
  , lerNextCursor :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

instance FromJSON ListEventsResponse where
  parseJSON = withObject "ListEventsResponse" $ \v ->
    ListEventsResponse
      <$> v .: "events"
      <*> v .:? "returned" .!= 0
      <*> v .:? "next_cursor"

-- | Response from listing notification channels.
data ListChannelsResponse = ListChannelsResponse
  { lcrChannels :: ![NotificationChannel]
  }
  deriving stock (Eq, Show)

instance FromJSON ListChannelsResponse where
  parseJSON = withObject "ListChannelsResponse" $ \v ->
    ListChannelsResponse
      <$> v .: "notification_channels"

-- | Response from listing project keys.
data ListKeysResponse = ListKeysResponse
  { lkrKeys :: ![ProjectKey]
  , lkrCount :: !Int
  }
  deriving stock (Eq, Show)

instance FromJSON ListKeysResponse where
  parseJSON = withObject "ListKeysResponse" $ \v ->
    ListKeysResponse
      <$> v .: "keys"
      <*> v .:? "count" .!= 0

-- | Response from batch getting breaker states.
data BatchBreakerStatesResponse = BatchBreakerStatesResponse
  { bbsStates :: ![AdminBreakerState]
  }
  deriving stock (Eq, Show)

instance FromJSON BatchBreakerStatesResponse where
  parseJSON = withObject "BatchBreakerStatesResponse" $ \v ->
    BatchBreakerStatesResponse
      <$> v .: "states"

-- ---------------------------------------------------------------------------
-- Pagination
-- ---------------------------------------------------------------------------

-- | Parameters for list endpoints.
data ListParams = ListParams
  { lpCursor :: !(Maybe Text)
  , lpLimit :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

-- | Default list parameters.
defaultListParams :: ListParams
defaultListParams = ListParams Nothing Nothing
