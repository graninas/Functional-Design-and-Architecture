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
nozzle1p = ComponentIndex "nozzle1-t"
nozzle1t = ComponentIndex "nozzle1-p"

nozzle2p, nozzle2t :: ComponentIndex
nozzle2p = ComponentIndex "nozzle2-t"
nozzle2t = ComponentIndex "nozzle2-p"


createBoosters :: Hdl Boosters
createBoosters = do
  lCtrl <- setupController lBooster lBoosterController aaaController86Passport
  registerComponent lCtrl nozzle1p aaaTemperature25Passport
  registerComponent lCtrl nozzle1t aaaPressure02Passport

  rCtrl <- setupController rBooster rBoosterController aaaController86Passport
  registerComponent rCtrl nozzle2p aaaTemperature25Passport
  registerComponent rCtrl nozzle2t aaaPressure02Passport
  pure (lCtrl, rCtrl)
