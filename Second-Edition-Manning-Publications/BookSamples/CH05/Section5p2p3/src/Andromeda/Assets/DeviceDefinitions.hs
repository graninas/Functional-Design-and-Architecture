module Andromeda.Assets.DeviceDefinitions where

import Andromeda.Hardware
import Andromeda.LogicControl
import Andromeda.Assets.Vendors.AAA.Components
import Andromeda.Common.Value


boostersDef :: Hdl (Hdl ())
boostersDef =
  [ SetupController "left booster" "left b ctrl" aaaController86Passport
    ( \lCtrl ->
      [ RegisterComponent lCtrl "nozzle1-t" aaaTemperature25Passport
      , RegisterComponent lCtrl "nozzle1-p" aaaPressure02Passport
      ]
    )
  , SetupController "right booster" "right b ctrl" aaaController86Passport
    ( \rCtrl ->
      [ RegisterComponent rCtrl "nozzle2-t" aaaTemperature25Passport
      , RegisterComponent rCtrl "nozzle2-p" aaaPressure02Passport
      ]
    )
  ]
