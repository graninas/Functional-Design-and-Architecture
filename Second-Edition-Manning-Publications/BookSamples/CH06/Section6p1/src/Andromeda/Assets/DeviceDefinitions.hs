module Andromeda.Assets.DeviceDefinitions where

import Andromeda.Hardware
import Andromeda.Assets.Vendors.AAA.Components


import Andromeda.Hardware
import Andromeda.Assets.Vendors.AAA.Components


createBoosters :: Hdl Controller
createBoosters = do
  t1   <- setupComponent aaaTemperature25Passport
  p1   <- setupComponent aaaPressure02Passport
  t2   <- setupComponent aaaTemperature25Passport
  p2   <- setupComponent aaaPressure02Passport
  ctrl <- setupController "controller" aaaController86Passport
  registerComponent ctrl "nozzle1-t" t1
  registerComponent ctrl "nozzle1-p" p1
  registerComponent ctrl "nozzle2-t" t2
  registerComponent ctrl "nozzle2-p" p2
  pure ctrl
