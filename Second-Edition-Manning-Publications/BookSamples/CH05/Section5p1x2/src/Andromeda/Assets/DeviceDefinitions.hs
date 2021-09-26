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






script :: LogicControl
script =
  [ EvalHdl (setupLCtrl (\lCtrl ->
    [ EvalHdl (setupLBooster lCtrl)
    , EvalDeviceControl (readTemperature lCtrl (\eTemp ->
      [ Report (show eTemp)
      ]
      ))
    ]
    ))
  ]

setupLCtrl :: (Controller -> LogicControl) -> Hdl LogicControl
setupLCtrl next =
  [ SetupController "left booster" "left b ctrl" aaaController86Passport
    ( \lCtrl -> next lCtrl
    )
  ]

setupLBooster :: Controller -> Hdl LogicControl
setupLBooster lCtrl =
  [ RegisterComponent lCtrl "nozzle1-t" aaaTemperature25Passport
  , RegisterComponent lCtrl "nozzle1-p" aaaPressure02Passport
  ]

readTemperature
  :: Controller
  -> (Either String Measurement -> LogicControl)
  -> DeviceControl LogicControl
readTemperature ctrl next =
  [ ReadSensor ctrl "nozzle1-t" next
  ]
