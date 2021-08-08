module Andromeda.Hardware.Vendors.Components where

import Andromeda.Hardware.Components.API
import Andromeda.Hardware.Common

import Data.Map (Map)

data VendorComponent
  = VendoredSensor     ComponentPassport SensorAPI
  | VendoredController ComponentPassport ControllerAPI


type VendorComponents = Map ComponentName VendorComponent
