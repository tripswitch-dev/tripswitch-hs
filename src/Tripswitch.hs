-- |
-- Module      : Tripswitch
-- Description : Tripswitch Haskell SDK — circuit breaker management.
--
-- This module re-exports everything a typical user needs from Layer 1.
-- Import this module for a complete circuit breaker API:
--
-- @
-- import Tripswitch
--
-- main :: IO ()
-- main = do
--   let cfg = defaultConfig
--         { cfgProjectID = \"proj_123\"
--         , cfgApiKey    = \"eb_live_...\"
--         }
--   withClient cfg $ \\client -> do
--     result <- execute client defaultExecConfig
--       { ecBreakers = [\"payment-latency\"]
--       , ecRouterID = \"main-router\"
--       , ecMetrics  = Map.singleton \"latency\" MetricLatency
--       }
--       myDatabaseCall
--     case result of
--       Left ErrBreakerOpen -> putStrLn \"Circuit breaker is open!\"
--       Right val           -> print val
-- @
module Tripswitch
  ( -- * Client
    Client
  , ClientConfig (..)
  , defaultConfig
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
  ) where

import Control.Exception (bracket)

import Tripswitch.Client hiding (newClient, withClient)
import qualified Tripswitch.Client as C
import Tripswitch.Client.Internal (startBackgroundThreads)

-- | Create a new client, start background threads (SSE, flusher, metadata sync),
-- and block until SSE is ready (5s timeout). This is the recommended constructor.
newClient :: ClientConfig -> IO Client
newClient cfg = do
  client <- C.newClient cfg
  startBackgroundThreads client
  pure client

-- | Create a client with 'bracket' to guarantee cleanup.
withClient :: ClientConfig -> (Client -> IO a) -> IO a
withClient cfg = bracket (newClient cfg) closeClient
