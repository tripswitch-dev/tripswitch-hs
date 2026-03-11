{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Type-safe breaker interactions using the GADT state machine (Layer 3).
--
-- The compiler ensures you can only execute through a breaker that has been
-- verified as allowing traffic.
module Main (main) where

import Tripswitch
import Tripswitch.Typed

main :: IO ()
main = do
  let cfg =
        defaultConfig
          { cfgProjectID = "proj_123"
          , cfgSSEDisabled = True
          , cfgFlusherDisabled = True
          , cfgMetaSyncDisabled = True
          }

  withClient cfg $ \client -> do
    -- Check the breaker — get a typed result
    result <- checkBreaker client "payment-latency"
    case result of
      Left blocked -> do
        -- 'blocked' is TBreaker 'POpen — the compiler knows it's open
        putStrLn $ "Breaker " <> show (breakerPhase blocked) <> " is open"

      Right witness -> do
        -- 'witness' is AllowWitness — proof that traffic is allowed
        -- Now we can safely execute
        val <- executeTyped client witness defaultExecConfig (pure (42 :: Int))
        putStrLn $ "Executed with result: " <> show val
