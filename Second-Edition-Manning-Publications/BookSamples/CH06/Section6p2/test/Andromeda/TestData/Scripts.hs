module Andromeda.TestData.Scripts where

import Andromeda

import Andromeda.Assets (createBoosters, aaaController86Name)
import Andromeda.Assets.Vendors.AAA.HardwareService (aaaHardwareService)
import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Language.DeviceControl as L
import qualified Andromeda.LogicControl.Language as L

import qualified Data.Map as Map


createBoosters :: Hdl (Controller, Controller)
createBoosters = do
  lCtrl <- L.setupController lBooster lBoosterController aaaController86Passport
  L.registerComponent lCtrl nozzle1p aaaTemperature25Passport
  L.registerComponent lCtrl nozzle1t aaaPressure02Passport

  rCtrl <- L.setupController rBooster rBoosterController aaaController86Passport
  L.registerComponent rCtrl nozzle2p aaaTemperature25Passport
  L.registerComponent rCtrl nozzle2t aaaPressure02Passport
  pure (lCtrl, rCtrl)


getControllerStatus :: Controller -> LogicControl (Either HardwareFailure ControllerStatus)
getControllerStatus ctrl = L.evalDeviceControl $ L.getStatus ctrl
