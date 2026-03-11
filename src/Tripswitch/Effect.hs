{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Tripswitch.Effect
-- Description : Layer 2 — CircuitBreaker effect for effectful.
module Tripswitch.Effect
  ( -- * Effect
    CircuitBreaker (..)

    -- * Operations
  , cbExecute
  , cbReport
  , cbGetBreakerState
  ) where

import Effectful (Dispatch (..), DispatchOf, Effect)
import Effectful.TH (makeEffect)

import Data.Text (Text)
import Tripswitch.Client
  ( BreakerState
  , ExecConfig
  , ReportInput
  , TripSwitchError
  )

-- | The CircuitBreaker effect.
type instance DispatchOf CircuitBreaker = 'Dynamic

-- | Circuit breaker effect — declares that a computation interacts
-- with a TripSwitch circuit breaker.
data CircuitBreaker :: Effect where
  CbExecute :: ExecConfig -> m a -> CircuitBreaker m (Either TripSwitchError a)
  CbReport :: ReportInput -> CircuitBreaker m ()
  CbGetBreakerState :: Text -> CircuitBreaker m (Maybe BreakerState)

makeEffect ''CircuitBreaker
