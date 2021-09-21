module Andromeda.HardwareSpec where

import Test.Hspec

import Andromeda

import Andromeda.Assets (createBoosters, aaaController86Name)
import Andromeda.Assets.Vendors.AAA.HardwareService (aaaHardwareService)
-- import Andromeda.Test.HardwareService (mockedHardwareService)
import Andromeda.TestData.Components (thermometer1Passp, pressure1Passp)

import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl
import qualified Andromeda.Hardware.Impl.HdlInterpreter as HdlImpl
import qualified Andromeda.Hardware.Impl.HilInterpreter as HilImpl
import qualified Andromeda.Hardware.Language.Hdl as L

import Data.IORef
import qualified Data.Map as Map



verifyTemperature :: Float -> SensorAPI -> IO ()
verifyTemperature temp handler = do
  measurement <- readMeasurement handler
  measurement `shouldBe` (Measurement Temperature temp)


getDevice :: RImpl.HardwareRuntime -> Controller -> IO TImpl.Device
getDevice RImpl.HardwareRuntime {devicesRef} ctrl = do
  devices <- readIORef devicesRef
  case Map.lookup ctrl devices of
    Nothing -> fail "Controller not found"
    Just device -> pure device

getDevicePart'
  :: RImpl.HardwareRuntime
  -> SImpl.HardwareService
  -> ComponentIndex
  -> Controller
  -> IO (Maybe TImpl.DevicePart)
getDevicePart' runtime service idx ctrl = do
  device <- getDevice runtime ctrl
  SImpl.getDevicePart service idx device


spec :: Spec
spec =
  describe "Hardware tests" $ do

    it "Hardware device components check" $ do

      runtime <- RImpl.createHardwareRuntime

      (leftBoosterCtrl, rightBoosterCtrl) <- HdlImpl.runHdl runtime aaaHardwareService createBoosters

      mbThermometer1 <- getDevicePart' runtime aaaHardwareService "nozzle1-t" leftBoosterCtrl
      mbThermometer2 <- getDevicePart' runtime aaaHardwareService "nozzle2-t" rightBoosterCtrl

      case (mbThermometer1, mbThermometer2) of
        (Nothing, _) -> fail "There is no such component"
        (_, Just _) -> fail "Found an unexpected thermometer"
        (Just thermometer, Nothing) -> putStrLn "Component found."

    it "Hardware device component method run" $ do

      runtime <- RImpl.createHardwareRuntime

      (leftBoosterCtrl, _) <- HdlImpl.runHdl runtime aaaHardwareService createBoosters
      mbThermometer <- getDevicePart' runtime aaaHardwareService "nozzle1-t" leftBoosterCtrl

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
