module Andromeda.Assets.DeviceDefinitions where

import Andromeda.Hardware
import Andromeda.Assets.Vendors.AAA.Components

type Boosters = (Controller, Controller)

createBoosters :: Hdl Boosters
createBoosters = do
  lCtrl <- setupController "left booster" "left b ctrl" aaaController86Passport
  registerComponent lCtrl "nozzle1-t" aaaTemperature25Passport
  registerComponent lCtrl "nozzle1-p" aaaPressure02Passport

  rCtrl <- setupController "right booster" "right b ctrl" aaaController86Passport
  registerComponent rCtrl "nozzle2-t" aaaTemperature25Passport
  registerComponent rCtrl "nozzle2-p" aaaPressure02Passport
  pure (lCtrl, rCtrl)
