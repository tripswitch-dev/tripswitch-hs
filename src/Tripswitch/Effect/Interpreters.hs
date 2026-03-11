{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Tripswitch.Effect.Interpreters
-- Description : IO and pure test interpreters for the CircuitBreaker effect.
module Tripswitch.Effect.Interpreters
  ( -- * IO Interpreter
    runCircuitBreakerIO

    -- * Pure Test Interpreter
  , runCircuitBreakerPure
  , PureState (..)
  , defaultPureState
  , CapturedSample (..)
  ) where

import Data.IORef (IORef, modifyIORef', readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO)
import System.Random (randomRIO)

import Tripswitch.Client
  ( BreakerState (..)
  , Client
  , ExecConfig (..)
  , ReportInput (..)
  , TripSwitchError (..)
  , execute
  , getState
  , report
  )
import Tripswitch.Client (BreakerStatus (..))
import Tripswitch.Effect (CircuitBreaker (..))

-- ---------------------------------------------------------------------------
-- IO Interpreter
-- ---------------------------------------------------------------------------

-- | Run the CircuitBreaker effect using a real TripSwitch 'Client'.
runCircuitBreakerIO
  :: (IOE :> es)
  => Client
  -> Eff (CircuitBreaker : es) a
  -> Eff es a
runCircuitBreakerIO client = interpret $ \env -> \case
  CbExecute cfg action -> localSeqUnliftIO env $ \unlift -> do
    execute client cfg (unlift action)
  CbReport ri -> localSeqUnliftIO env $ \_ ->
    report client ri
  CbGetBreakerState name -> localSeqUnliftIO env $ \_ -> do
    mStatus <- getState client name
    pure $ fmap bsState mStatus

-- ---------------------------------------------------------------------------
-- Pure Test Interpreter
-- ---------------------------------------------------------------------------

-- | State for the pure test interpreter.
data PureState = PureState
  { psBreakerStates :: !(Map Text BreakerState)
  , psCapturedSamples :: ![CapturedSample]
  }
  deriving stock (Eq, Show)

-- | A captured sample from the pure interpreter.
data CapturedSample = CapturedSample
  { csRouterID :: !Text
  , csMetric :: !Text
  , csValue :: !Double
  , csOK :: !Bool
  , csTags :: !(Map Text Text)
  , csTraceID :: !Text
  }
  deriving stock (Eq, Show)

-- | Default pure state with no breakers and no captured samples.
defaultPureState :: PureState
defaultPureState = PureState Map.empty []

-- | Run the CircuitBreaker effect purely, using injected breaker states.
runCircuitBreakerPure
  :: (IOE :> es)
  => IORef PureState
  -> Eff (CircuitBreaker : es) a
  -> Eff es a
runCircuitBreakerPure stateRef = interpret $ \env -> \case
  CbExecute cfg action -> localSeqUnliftIO env $ \unlift -> do
    gateResult <- checkBreakersPure stateRef cfg
    case gateResult of
      Left err -> pure (Left err)
      Right () -> Right <$> unlift action

  CbReport ri -> localSeqUnliftIO env $ \_ -> do
    let sample =
          CapturedSample
            { csRouterID = riRouterID ri
            , csMetric = riMetric ri
            , csValue = riValue ri
            , csOK = riOK ri
            , csTags = riTags ri
            , csTraceID = riTraceID ri
            }
    modifyIORef' stateRef $ \s ->
      s {psCapturedSamples = psCapturedSamples s <> [sample]}

  CbGetBreakerState name -> localSeqUnliftIO env $ \_ -> do
    st <- readIORef stateRef
    pure $ Map.lookup name (psBreakerStates st)

-- | Check breaker states in the pure interpreter.
checkBreakersPure :: IORef PureState -> ExecConfig -> IO (Either TripSwitchError ())
checkBreakersPure stateRef cfg
  | null (ecBreakers cfg) = pure (Right ())
  | otherwise = do
      st <- readIORef stateRef
      let states = psBreakerStates st
          check [] minRate = Right minRate
          check (b:bs) minRate = case Map.lookup b states of
            Nothing          -> check bs minRate
            Just Open        -> Left ErrBreakerOpen
            Just Closed      -> check bs minRate
            Just (HalfOpen r) -> check bs (min minRate r)
      case check (ecBreakers cfg) 1.0 of
        Left err -> pure (Left err)
        Right minRate
          | minRate >= 1.0 -> pure (Right ())
          | otherwise -> do
              roll <- randomRIO (0.0, 1.0)
              pure $ if roll < minRate then Right () else Left ErrBreakerOpen
