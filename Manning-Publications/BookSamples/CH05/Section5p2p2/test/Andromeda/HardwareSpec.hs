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
import qualified Andromeda.Hardware.Impl.HdlInterpreter as HdlImpl
import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Domain as D

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



testBoostersDef :: Hdl
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

      let devices = Map.empty

      devices' <- HdlImpl.runHdl devices aaaHardwareService testBoostersDef

      mbThermometer1 <- getDevicePart' devices' aaaHardwareService "nozzle1-t" "left b ctrl"
      mbThermometer2 <- getDevicePart' devices' aaaHardwareService "nozzle2-t" "right b ctrl"

      mbNonExistentTherm1 <- getDevicePart' devices' aaaHardwareService "xxx-t" "left b ctrl"
      mbNonExistentTherm2 <- getDevicePart' devices' aaaHardwareService "xxx-t" "right b ctrl"

      case (mbNonExistentTherm1, mbNonExistentTherm2) of
        (Nothing, Nothing) -> pure ()
        _ -> fail "Found an unexpected thermometer"

      case (mbThermometer1, mbThermometer2) of
        (Just therm1, Just therm2) -> putStrLn "Component found."
        _ -> fail "There is no such component"

    it "Hardware device component method run" $ do

      let devices = Map.empty

      devices' <- HdlImpl.runHdl devices aaaHardwareService testBoostersDef
      mbThermometer <- getDevicePart' devices' aaaHardwareService "nozzle1-t" "left b ctrl"

      case mbThermometer of
        Nothing -> fail "There is no such component"
        Just thermometer -> TImpl.withHandler thermometer (verifyTemperature 100.0)
