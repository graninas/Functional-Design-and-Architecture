module Andromeda.Hardware.Common where

import Andromeda.Components.API (SensorAPI, ControllerAPI)


-- Every physical instance of a component has its own GUID
type PhysicalGuid = String

-- Components are grouped by name,
-- for example, temperature sensors AAA-T-25
type ComponentName = String

data Parameter      = Temperature | Pressure
data ComponentClass = Sensors | Controllers
type VendorName     = String

data ComponentDef = ComponentDef
  { componentClass  :: ComponentClass
  , componentName   :: ComponentName
  , componentGuid   :: PhysicalGuid
  , componentVendor :: VendorName
  }


data Measurement = Measurement Parameter Float
data Period = Secondly




data VendorComponent
  = VendoredSensor     ComponentDef SensorAPI
  | VendoredController ComponentDef ControllerAPI


type VendorComponents = Map (VendorName, ComponentName) DeviceComponent
