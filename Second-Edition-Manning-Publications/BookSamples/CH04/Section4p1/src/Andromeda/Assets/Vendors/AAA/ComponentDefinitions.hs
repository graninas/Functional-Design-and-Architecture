module Andromeda.Assets.Vendors.AAA.ComponentDefinitions where

import Andromeda.Hardware.Common

import qualified Data.Map as Map

-- import Native AAA sensors library here or do FFI

aaaInc :: Vendor
aaaInc = "AAA Inc."


aaaTemperature25Name :: ComponentName
aaaTemperature25Name = "AAA-T-25"

aaaPressure02Name :: ComponentName
aaaPressure02Name = "AAA-З-02"

aaaController86Name :: ComponentName
aaaController86Name = "AAA-C-86"


guid1 = "some_guid1"
guid2 = "some_guid2"
guid3 = "some_guid3"


aaaTemperature25Passport = ComponentPassport Sensors     aaaTemperature25Name guid2 aaaInc
aaaPressure02Passport    = ComponentPassport Sensors     aaaPressure02Name    guid1 aaaInc
aaaController86Passport  = ComponentPassport Controllers aaaController86Name  guid3 aaaInc
