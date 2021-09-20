module Andromeda.Assets.DeviceDefinitions where

import Andromeda.Hardware
import Andromeda.Assets.Vendors.AAA.Components

type Boosters = (Controller, Controller)

createBoosters :: Hdl Boosters
createBoosters = do
  ctrl1 <- setupController "left booster" "left b ctrl" aaaController86Passport
  registerComponent ctrl1 "nozzle1-t" aaaTemperature25Passport
  registerComponent ctrl1 "nozzle1-p" aaaPressure02Passport

  ctrl2 <- setupController "right booster" "right b ctrl" aaaController86Passport
  registerComponent ctrl2 "nozzle2-t" aaaTemperature25Passport
  registerComponent ctrl2 "nozzle2-p" aaaPressure02Passport
  pure (ctrl1, ctrl2)
