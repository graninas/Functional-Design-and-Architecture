module Andromeda.Assets.Hardware.Components where

import Andromeda.Hardware
import Andromeda.Vendors.AAA

guid1 = "guid1"
guid2 = "guid2"
guid3 = "guid3"



aaa_p_02 = ComponentDef Sensors     p02SensorName guid1 "AAA Inc."
aaa_t_25 = ComponentDef Sensors     t25SensorName guid2 "AAA Inc."
aaa_c_86 = ComponentDef Controllers c86ControllerName guid3 "AAA Inc."


boostersDef :: Hdl
boostersDef =
  [ Sensor aaa_t_25 "nozzle1-t" Temperature
  , Sensor aaa_p_02 "nozzle1-p" Pressure
  , Sensor aaa_t_25 "nozzle2-t" Temperature
  , Sensor aaa_p_02 "nozzle2-p" Pressure
  , Controller aaa_c_86 "controller"
  ]





-- Inconsistency allowed:

-- Defined in Andromeda.Hardware.Components.Assets
-- t25Sensor:: ComponentDef
-- t25Sensor =
--   ComponentDef Sensors "some_guid" "AAA Inc." "AAA-T-25"

validComponent :: Component
validComponent =
  Sensor t25Sensor "nozzle1-t" Temperature

invalidComponent :: Component
invalidComponent =
  Controller t25Sensor "central controller"

--           ^ a sensor passed by a mistake
