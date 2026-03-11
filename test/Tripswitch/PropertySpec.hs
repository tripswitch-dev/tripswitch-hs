module Tripswitch.PropertySpec (spec) where

import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Map.Strict as Map

import Tripswitch.Client (mergeTags)

spec :: Spec
spec = do
  describe "Tag merge properties" $ do
    it "per-call tags override global tags" $ hedgehog $ do
      key <- Hedgehog.forAll $ Gen.text (Range.linear 1 10) Gen.alpha
      globalVal <- Hedgehog.forAll $ Gen.text (Range.linear 1 10) Gen.alpha
      localVal <- Hedgehog.forAll $ Gen.text (Range.linear 1 10) Gen.alpha
      let global = Map.singleton key globalVal
          local = Map.singleton key localVal
          merged = mergeTags global local
      Map.lookup key merged Hedgehog.=== Just localVal

    it "merge is superset of both inputs" $ hedgehog $ do
      globalKeys <- Hedgehog.forAll $ Gen.list (Range.linear 0 5) (Gen.text (Range.linear 1 5) Gen.alpha)
      localKeys <- Hedgehog.forAll $ Gen.list (Range.linear 0 5) (Gen.text (Range.linear 1 5) Gen.alpha)
      let global = Map.fromList [(k, "g") | k <- globalKeys]
          local = Map.fromList [(k, "l") | k <- localKeys]
          merged = mergeTags global local
      -- All keys from both maps should be present
      Hedgehog.assert $ all (`Map.member` merged) globalKeys
      Hedgehog.assert $ all (`Map.member` merged) localKeys
