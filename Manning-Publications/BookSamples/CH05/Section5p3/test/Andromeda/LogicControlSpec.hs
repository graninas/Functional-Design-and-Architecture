module Andromeda.LogicControlSpec where

import Test.Hspec

import Andromeda

import Andromeda.Assets (boostersDef, aaaController86Name)
import Andromeda.Assets.Vendors.AAA.HardwareService (aaaHardwareService)
import Andromeda.TestData.Components (thermometer1Passp, pressure1Passp)

import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl
import qualified Andromeda.Hardware.Impl.Interpreters.Hdl as HdlImpl
import qualified Andromeda.Hardware.Language.Hdl as L

import qualified Andromeda.LogicControl.Impl.Interpreters.LogicControl as LCImpl

import qualified Data.Map as Map


verifyTemperature :: Float -> SensorAPI -> IO ()
verifyTemperature temp handler = do
  measurement <- readMeasurement handler
  measurement `shouldBe` (Measurement Temperature temp)


getDevice :: RImpl.Runtime -> ControllerName -> IO TImpl.Device
getDevice runtime ctrlName = do
  let devices = RImpl._devices runtime
  case Map.lookup (Controller ctrlName) devices of
    Nothing -> fail "Controller not found"
    Just (_, device) -> pure device

getDevicePart'
  :: RImpl.Runtime
  -> ComponentIndex
  -> ControllerName
  -> IO (Maybe TImpl.DevicePart)
getDevicePart' runtime  idx ctrlName = do
  let service = RImpl._hardwareService runtime
  device <- getDevice runtime ctrlName
  SImpl.getDevicePart service idx device







-- Listing 5.5
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



testBoostersDef :: Hdl (Hdl (Hdl ()))
testBoostersDef =
  [ SetupController "left booster" "left b ctrl" aaaController86Passport
    ( \lCtrl ->
      [ RegisterComponent lCtrl "nozzle1-t" aaaTemperature25Passport
      , RegisterComponent lCtrl "nozzle1-p" aaaPressure02Passport
      , SetupController "right booster" "right b ctrl" aaaController86Passport
        ( \rCtrl ->
          [ RegisterComponent rCtrl "nozzle2-t" aaaTemperature25Passport
          , RegisterComponent rCtrl "nozzle2-p" aaaPressure02Passport
          ]
        )
      ]
    )
  ]



spec :: Spec
spec =
  describe "Hardware tests" $ do

    it "Hardware device components check" $ do

      let runtime = RImpl.Runtime Map.empty aaaHardwareService
      runtime' <- LCImpl.runLogicControl runtime testScript

      mbTherm  <- getDevicePart' runtime' "therm" "ctrl"

      case mbTherm of
        Nothing          -> fail "There is no such component"
        Just thermometer -> putStrLn "Component found."

    it "Hardware device component method run" $ do

      let runtime = RImpl.Runtime Map.empty aaaHardwareService
      runtime' <- LCImpl.runLogicControl runtime testScript
      mbTherm  <- getDevicePart' runtime' "therm" "ctrl"

      case mbTherm of
        Nothing          -> fail "There is no such component"
        Just thermometer -> TImpl.withHandler thermometer (verifyTemperature 100.0)
