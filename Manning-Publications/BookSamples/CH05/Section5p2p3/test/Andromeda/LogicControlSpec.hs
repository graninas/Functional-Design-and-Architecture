module Andromeda.LogicControlSpec where

import Test.Hspec

import Andromeda

import Andromeda.Assets (boostersDef, script, aaaController86Name)
import Andromeda.Assets.Vendors.AAA.HardwareService (aaaHardwareService)
-- import Andromeda.Test.HardwareService (mockedHardwareService)
import Andromeda.TestData.Components (thermometer1Passp, pressure1Passp)

import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl
import qualified Andromeda.Hardware.Impl.Interpreters.Hdl as HdlImpl
import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Domain as D

import qualified Andromeda.LogicControl.Impl.Interpreters.LogicControl as LCImpl

import qualified Data.Map as Map


verifyTemperature :: Float -> SensorAPI -> IO ()
verifyTemperature temp handler = do
  measurement <- readMeasurement handler
  measurement `shouldBe` (Measurement Temperature temp)


getDevice :: RImpl.Devices -> ControllerName -> IO TImpl.Device
getDevice devices ctrlName = case Map.lookup (D.Controller ctrlName) devices of
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


testScript :: LogicControl
testScript =
  [ EvalHdl
    [ SetupController "device" "ctrl" aaaController86Passport (\ctrl ->
      [ EvalHdl
        [ RegisterComponent ctrl "therm" aaaTemperature25Passport ]
      , EvalDeviceControl (readAndReport ctrl)
      ]
    )]
  ]
  where
    readAndReport :: Controller -> DeviceControl LogicControl
    readAndReport ctrl =
      [ ReadSensor ctrl "therm" (\eMeasurement ->
        [ Report (show eMeasurement) ])
      ]


spec :: Spec
spec =
  describe "Hardware tests" $ do

    it "Hardware device components check" $ do

      devices <- LCImpl.runLogicControl Map.empty aaaHardwareService testScript
      mbTherm  <- getDevicePart' devices aaaHardwareService "therm" "ctrl"

      case mbTherm of
        Nothing          -> fail "There is no such component"
        Just thermometer -> putStrLn "Component found."

    it "Hardware device component method run" $ do

      devices' <- LCImpl.runLogicControl Map.empty aaaHardwareService testScript
      mbTherm  <- getDevicePart' devices' aaaHardwareService "therm" "ctrl"

      case mbTherm of
        Nothing          -> fail "There is no such component"
        Just thermometer -> TImpl.withHandler thermometer (verifyTemperature 100.0)
