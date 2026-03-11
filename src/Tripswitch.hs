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

import Tripswitch.Client
