module Andromeda.Assets.DeviceDefinitions where

import Andromeda.Hardware
import Andromeda.Assets.Vendors.AAA.Components

type Boosters = (Controller, Controller)

lBooster :: DeviceName
lBooster = DeviceName "left booster"

rBooster :: DeviceName
rBooster = DeviceName "right booster"

lBoosterController :: ControllerName
lBoosterController = ControllerName "left b ctrl"

rBoosterController :: ControllerName
rBoosterController = ControllerName "right b ctrl"

nozzle1p, nozzle1t :: ComponentIndex
nozzle1p = ComponentIndex "nozzle1-p"
nozzle1t = ComponentIndex "nozzle1-t"

nozzle2p, nozzle2t :: ComponentIndex
nozzle2p = ComponentIndex "nozzle2-p"
nozzle2t = ComponentIndex "nozzle2-t"


createBoosters :: Hdl Boosters
createBoosters = do
  lCtrl <- setupController lBooster lBoosterController aaaController86Passport
  registerComponent lCtrl nozzle1p aaaPressure02Passport
  registerComponent lCtrl nozzle1t aaaTemperature25Passport

  rCtrl <- setupController rBooster rBoosterController aaaController86Passport
  registerComponent rCtrl nozzle2p aaaPressure02Passport
  registerComponent rCtrl nozzle2t aaaTemperature25Passport
  pure (lCtrl, rCtrl)



rotaryThruster :: DeviceName
rotaryThruster = DeviceName "rotary thruster"

rotaryThrusterController :: ControllerName
rotaryThrusterController = ControllerName "rotary thruster ctrl"

mainEngine :: DeviceName
mainEngine = DeviceName "main engine"

mainEngineController :: ControllerName
mainEngineController = ControllerName "main engine ctrl"
