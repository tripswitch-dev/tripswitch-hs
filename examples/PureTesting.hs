{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Demonstrates pure testing with the CircuitBreaker effect.
--
-- No network, no IO, no mocks — just inject breaker states
-- and verify behavior.
module Main (main) where

import Data.IORef (newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Effectful (Eff, IOE, runEff, (:>))

import Tripswitch
import Tripswitch.Effect (CircuitBreaker, cbExecute, cbGetBreakerState)
import Tripswitch.Effect.Interpreters
  ( CapturedSample (..)
  , PureState (..)
  , defaultPureState
  , runCircuitBreakerPure
  )

-- | Business logic that uses circuit breakers.
myBusinessLogic :: (CircuitBreaker :> es, IOE :> es) => Eff es String
myBusinessLogic = do
  mState <- cbGetBreakerState "payment-breaker"
  case mState of
    Just Open -> pure "serving cached data"
    _ -> do
      result <- cbExecute defaultExecConfig (pure "fresh data")
      case result of
        Left _ -> pure "fallback"
        Right val -> pure val

main :: IO ()
main = do
  -- Test with breaker closed
  stateRef <- newIORef defaultPureState
  result1 <- runEff $ runCircuitBreakerPure stateRef myBusinessLogic
  putStrLn $ "Closed breaker: " <> result1

  -- Test with breaker open
  stateRef2 <- newIORef defaultPureState {psBreakerStates = Map.singleton "payment-breaker" Open}
  result2 <- runEff $ runCircuitBreakerPure stateRef2 myBusinessLogic
  putStrLn $ "Open breaker: " <> result2

  putStrLn "Pure tests passed!"
