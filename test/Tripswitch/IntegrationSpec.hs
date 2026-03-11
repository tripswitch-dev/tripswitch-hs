module Tripswitch.IntegrationSpec (spec) where

import System.Environment (lookupEnv)
import Test.Hspec

spec :: Spec
spec = do
  describe "Integration tests" $ do
    it "are gated by TRIPSWITCH_API_KEY env var" $ do
      mKey <- lookupEnv "TRIPSWITCH_API_KEY"
      case mKey of
        Nothing -> pendingWith "TRIPSWITCH_API_KEY not set"
        Just _ -> pendingWith "integration tests not yet implemented"
