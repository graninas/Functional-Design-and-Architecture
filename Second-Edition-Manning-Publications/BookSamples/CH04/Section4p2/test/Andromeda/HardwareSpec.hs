module Andromeda.HardwareSpec where

import Test.Hspec

import Andromeda

import Andromeda.Assets (boostersDef, aaaController86Name)
import Andromeda.Assets.Vendors.AAA.HardwareService (aaaHardwareService)
import Andromeda.Test.HardwareService (mockedHardwareService)
import Andromeda.TestData.Components (thermometer1Passp)

spec :: Spec
spec =
  describe "Hardware tests" $ do

    it "Hardware device components check" $ do

      boosters      <- makeDevice aaaHardwareService boostersDef
      mbThermometer <- getDevicePart aaaHardwareService "nozzle1-t" boosters

      case mbThermometer of
        Nothing -> fail "There is no such component"
        Just thermometer -> putStrLn "Component found."

    it "Hardware device component method run" $ do

      boosters      <- makeDevice aaaHardwareService boostersDef
      mbThermometer <- getDevicePart aaaHardwareService "nozzle1-t" boosters

      case mbThermometer of
        Nothing -> fail "There is no such component"
        Just thermometer -> do
          withHandler thermometer $ \handler -> do
            measurement <- readMeasurement handler
            measurement `shouldBe` (Measurement Temperature 100.0)

    it "Mocked device part" $ do

      let testDef =
            [ ComponentDef "t1" thermometer1Passp
            ]

      device <- makeDevice mockedHardwareService testDef
      mpPart <- getDevicePart mockedHardwareService "t1" device

      case mpPart of
        Nothing -> fail "There is no such part"
        Just part -> do
          withHandler part $ \handler -> do
            measurement <- readMeasurement handler
            measurement `shouldBe` (Measurement Temperature 50.0)
