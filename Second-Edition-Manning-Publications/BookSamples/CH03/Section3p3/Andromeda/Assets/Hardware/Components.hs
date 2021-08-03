module Andromeda.Assets.Hardware.Components
  (


  ) where

import Andromeda.Hardware

guid1 = "guid1"
guid2 = "guid2"
guid3 = "guid3"



aaa_p_02 = ComponentDef Sensors     guid1 "AAA Inc." "AAA-P-02"
aaa_t_25 = ComponentDef Sensors     guid2 "AAA Inc." "AAA-T-25"
aaa_c_86 = ComponentDef Controllers guid3 "AAA Inc." "AAA-C-86"


boostersDef :: Hdl
boostersDef =
  [ Sensor aaa_t_25 "nozzle1-t" Temperature
  , Sensor aaa_p_02 "nozzle1-p" Pressure
  , Sensor aaa_t_25 "nozzle2-t" Temperature
  , Sensor aaa_p_02 "nozzle2-p" Pressure
  , Controller aaa_c_86 "controller"
  ]





-- Inconsistency allowed:

t25Sensor:: ComponentDef
t25Sensor =
  ComponentDef Sensors "some_guid" "AAA Inc." "AAA-T-25"

validComponent :: Component
validComponent =
  Sensor t25Sensor "nozzle1-t" Temperature

invalidComponent :: Component
invalidComponent =
  Controller t25Sensor "central controller"

--           ^ a sensor passed by a mistake
