{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- |
-- Module      : Tripswitch.Typed
-- Description : Layer 3 — GADT state machine for type-safe breaker interactions.
module Tripswitch.Typed
  ( -- * Phase type
    Phase (..)

    -- * Typed breaker
  , TBreaker (..)

    -- * Witnesses
  , AllowWitness (..)

    -- * Operations
  , checkBreaker
  , executeTyped

    -- * Inspection
  , breakerPhase
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM (readTVarIO)
import System.Random (randomRIO)

import Tripswitch.Client
  ( BreakerState (..)
  , Client (..)
  , ExecConfig
  , execute
  )

data Phase = PClosed | POpen | PHalfOpen

type TBreaker :: Phase -> Type
data TBreaker (p :: Phase) where
  TClosed :: !Text -> TBreaker 'PClosed
  TOpen :: !Text -> TBreaker 'POpen
  THalfOpen :: !Text -> !Double -> TBreaker 'PHalfOpen

data AllowWitness where
  AllowClosed :: TBreaker 'PClosed -> AllowWitness
  AllowHalfOpen :: TBreaker 'PHalfOpen -> AllowWitness

checkBreaker :: Client -> Text -> IO (Either (TBreaker 'POpen) AllowWitness)
checkBreaker client name = do
  states <- readTVarIO (cBreakerStates client)
  case Map.lookup name states of
    Nothing ->
      pure $ Right $ AllowClosed (TClosed name)
    Just Closed ->
      pure $ Right $ AllowClosed (TClosed name)
    Just Open ->
      pure $ Left $ TOpen name
    Just (HalfOpen rate) -> do
      roll <- randomRIO (0.0 :: Double, 1.0)
      if roll < rate
        then pure $ Right $ AllowHalfOpen (THalfOpen name rate)
        else pure $ Left $ TOpen name

executeTyped :: Client -> AllowWitness -> ExecConfig -> IO a -> IO a
executeTyped client _witness cfg task = do
  result <- execute client cfg task
  case result of
    Left err -> error $ "executeTyped: unexpected SDK error: " <> show err
    Right val -> pure val

breakerPhase :: TBreaker p -> Text
breakerPhase (TClosed name) = name
breakerPhase (TOpen name) = name
breakerPhase (THalfOpen name _) = name
