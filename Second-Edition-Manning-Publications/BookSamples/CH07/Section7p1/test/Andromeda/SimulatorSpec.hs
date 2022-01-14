module Andromeda.SimulatorSpec where

import Test.Hspec

import Andromeda
















spec :: Spec
spec =
  describe "Logic Control tests" $ do

    it "Controller status check" $ do

      simControl <- startSimulator spaceshipModel

      simResult <- runSimulation leftBoosterMallfunction

      case simResult of
        Nothing -> error "Unexpected absence of results"
        Just () -> pure ()
