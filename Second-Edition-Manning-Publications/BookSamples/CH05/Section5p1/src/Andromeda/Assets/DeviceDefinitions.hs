module Andromeda.Assets.DeviceDefinitions where

import Andromeda.Hardware
import Andromeda.Assets.Vendors.AAA.Components


hdlScript :: Hdl
hdlScript =
  [ SetupController "left booster" "left b ctrl" aaaController86Passport

  , RegisterComponent "left b ctrl" "nozzle1-t" aaaTemperature25Passport
  , RegisterComponent "left b ctrl" "nozzle1-p" aaaPressure02Passport

  , SetupController "right booster" "right b ctrl" aaaController86Passport
  , RegisterComponent "right b ctrl" "nozzle2-t" aaaTemperature25Passport
  , RegisterComponent "right b ctrl" "nozzle2-p" aaaPressure02Passport

  , ReadSensor "nozzle1-t"
      ( \eMeasurement ->
        [ case eMeasurement of
            Left err -> Report err
            Right (Measurement _ val) -> Store "nozzle1-t temp" (floatValue val)
        ]
      )
  ]
