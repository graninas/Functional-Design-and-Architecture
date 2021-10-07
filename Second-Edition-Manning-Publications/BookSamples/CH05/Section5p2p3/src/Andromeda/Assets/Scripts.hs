module Andromeda.Assets.Scripts where

import Andromeda.Hardware
import Andromeda.LogicControl
import Andromeda.Assets.Vendors.AAA.Components
import Andromeda.Common.Value



-- Listing 5.5
script :: LogicControl
script =
  [ EvalHdl
    [ SetupController "device" "ctrl" aaaController86Passport (\ctrl ->
      [ EvalHdl
        [ RegisterComponent ctrl "therm" aaaTemperature25Passport ]
      , EvalDeviceControl (readAndReport ctrl)
      ]
    )]
  ]
  where
    readAndReport :: Controller -> DeviceControl LogicControl
    readAndReport ctrl =
      [ ReadSensor ctrl "therm" (\eMeasurement ->
        [ Report (show eMeasurement) ])
      ]
