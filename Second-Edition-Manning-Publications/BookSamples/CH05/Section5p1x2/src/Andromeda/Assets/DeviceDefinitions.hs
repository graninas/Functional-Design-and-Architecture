module Andromeda.Assets.DeviceDefinitions where

import Andromeda.Hardware
import Andromeda.Assets.Vendors.AAA.Components
import Andromeda.Common.Value


boostersDef :: Hdl
boostersDef =
  [ SetupController "left booster" "left b ctrl" aaaController86Passport
  , RegisterComponent "left b ctrl" "nozzle1-t" aaaTemperature25Passport
  , RegisterComponent "left b ctrl" "nozzle1-p" aaaPressure02Passport

  , SetupController "right booster" "right b ctrl" aaaController86Passport
  , RegisterComponent "right b ctrl" "nozzle2-t" aaaTemperature25Passport
  , RegisterComponent "right b ctrl" "nozzle2-p" aaaPressure02Passport
  ]
