
module Andromeda.HardwareSpec where

import           Test.Hspec

import Andromeda

import Andromeda.Assets (boostersDef, aaaController86Name)
import Andromeda.Assets.Vendors.AAA.ComponentsAPI (aaaVendorComponents)

spec :: Spec
spec =
  describe "Hardware tests" $ do

    it "Hardware device components check" $ do

      let boosters = makeDevice aaaVendorComponents boostersDef
      let mbThermometer = getDevicePart "nozzle1-t" boosters

      case mbThermometer of
        Nothing -> fail "There is no such component"
        Just thermometer -> putStrLn "Component found."

    it "Hardware device component method run" $ do

      let boosters = makeDevice aaaVendorComponents boostersDef
      let mbThermometer = getDevicePart "nozzle1-t" boosters

      case mbThermometer of
        Nothing -> fail "There is no such component"
        Just thermometer -> do
          withHandler thermometer $ \handler -> do
            measurement <- readMeasurement handler
            measurement `shouldBe` (Measurement Temperature 100.0)
