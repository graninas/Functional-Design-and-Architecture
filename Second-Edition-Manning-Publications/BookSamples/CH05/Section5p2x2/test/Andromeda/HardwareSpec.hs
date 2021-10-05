module Andromeda.HardwareSpec where

import Test.Hspec

import Andromeda

import Andromeda.Assets (boostersDef, aaaController86Name)
import Andromeda.Assets.Vendors.AAA.HardwareService (aaaHardwareService)
-- import Andromeda.Test.HardwareService (mockedHardwareService)
import Andromeda.TestData.Components (thermometer1Passp, pressure1Passp)

import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl
import qualified Andromeda.Hardware.Impl.Interpreters.Hdl as HdlImpl
import qualified Andromeda.Hardware.Language.Hdl as L

import qualified Data.Map as Map


verifyTemperature :: Float -> SensorAPI -> IO ()
verifyTemperature temp handler = do
  measurement <- readMeasurement handler
  measurement `shouldBe` (Measurement Temperature temp)


getDevice :: RImpl.Devices -> ControllerName -> IO TImpl.Device
getDevice devices ctrlName = case Map.lookup ctrlName devices of
  Nothing -> fail "Controller not found"
  Just (_, device) -> pure device

getDevicePart'
  :: RImpl.Devices
  -> SImpl.HardwareService
  -> ComponentIndex
  -> ControllerName
  -> IO (Maybe TImpl.DevicePart)
getDevicePart' devices service idx ctrlName = do
  device <- getDevice devices ctrlName
  SImpl.getDevicePart service idx device



hdlScript :: L.Hdl
hdlScript =
  [ L.ReadSensor "left b ctrl" "nozzle1-t"
      ( \eMeasurement ->
        [ case eMeasurement of
            Left err -> L.Report err
            Right (Measurement _ val) -> L.Store "nozzle1-t temp" (floatValue val)
        ]
      )
  ]


spec :: Spec
spec =
  describe "Hardware tests" $ do

    it "Hardware device components check" $ do

      let devices = Map.empty

      devices' <- HdlImpl.runHdl devices aaaHardwareService boostersDef

      mbThermometer1 <- getDevicePart' devices' aaaHardwareService "nozzle1-t" "left b ctrl"
      mbThermometer2 <- getDevicePart' devices' aaaHardwareService "nozzle2-t" "right b ctrl"

      case (mbThermometer1, mbThermometer2) of
        (Nothing, _) -> fail "There is no such component"
        (_, Just _) -> fail "Found an unexpected thermometer"
        (Just thermometer, Nothing) -> putStrLn "Component found."

    it "Hardware device component method run" $ do

      let devices = Map.empty

      devices' <- HdlImpl.runHdl devices aaaHardwareService boostersDef
      mbThermometer <- getDevicePart' devices' aaaHardwareService "nozzle1-t" "left b ctrl"

      case mbThermometer of
        Nothing -> fail "There is no such component"
        Just thermometer -> TImpl.withHandler thermometer (verifyTemperature 100.0)

    -- it "Getting measurement from mocked device" $ do
    --   runtime <- Impl.createHardwareRuntime
    --
    --   let createTestDevice = do
    --         ctrl <- L.setupController "test device" "controler" controller1Passp
    --         L.registerComponent "t1" thermometer1Passp
    --         pure ctrl
    --
    --   ctrl <- Impl.runHdl runtime mockedHardwareService createTestDevice
    --   mpPart <- Impl.getDevicePart mockedHardwareService "t1" ctrl
    --
    --   case mpPart of
    --     Nothing -> fail "There is no such part"
    --     Just part -> Impl.withHandler part (verifyTemperature 50.0)
    --
    --
    -- it "Getting absent device part" $ do
    --
    --   runtime <- Impl.createHardwareRuntime
    --
    --   let createTestDevice = do
    --         ctrl <- L.setupController "test device" "controler" controller1Passp
    --         L.registerComponent "t1" thermometer1Passp
    --         L.registerComponent "p1" pressure1Passp
    --         pure ctrl
    --
    --   ctrl <- Impl.runHdl runtime mockedHardwareService createTestDevice
    --   mpPart1 <- getDevicePart mockedHardwareService "t1" ctrl
    --   mpPart2 <- getDevicePart mockedHardwareService "p1" ctrl
    --   mpPart3 <- getDevicePart mockedHardwareService "t2" ctrl
    --
    --   case (mpPart1, mpPart2, mpPart3) of
    --     (Just _, Just _, Nothing) -> pure ()
    --     _ -> fail "Device is assembled incorrectly."
