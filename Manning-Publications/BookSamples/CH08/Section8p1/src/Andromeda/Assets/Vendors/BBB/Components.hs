module Andromeda.Assets.Vendors.BBB.Components where

import Andromeda.Hardware.Common
import Andromeda.Assets.Vendors.BBB.Common

import qualified Data.Map as Map

-- import Native BBB sensors library here or do FFI

bbbTemperature25Name :: ComponentName
bbbTemperature25Name = "BBB-T-25"

bbbPressure02Name :: ComponentName
bbbPressure02Name = "BBB-З-02"

bbbController86Name :: ComponentName
bbbController86Name = "BBB-C-86"


guid1 = "bbb_guid1"
guid2 = "bbb_guid2"
guid3 = "bbb_guid3"


bbbTemperature25Passport :: ComponentPassport
bbbTemperature25Passport = ComponentPassport (Sensors TemperatureSensor) bbbTemperature25Name guid2 bbbInc

bbbPressure02Passport :: ComponentPassport
bbbPressure02Passport = ComponentPassport (Sensors PressureSensor) bbbPressure02Name guid1 bbbInc

bbbController86Passport :: ComponentPassport
bbbController86Passport = ComponentPassport Controllers bbbController86Name guid3 bbbInc
