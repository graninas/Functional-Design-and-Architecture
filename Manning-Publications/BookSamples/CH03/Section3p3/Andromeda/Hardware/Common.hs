module Andromeda.Hardware.Common where

-- Every physical instance of a component has its own GUID
type PhysicalGuid = String

-- Components are grouped by name,
-- for example, temperature sensors AAA-T-25
type ComponentName = String

data Parameter      = Temperature | Pressure
data ComponentClass = Sensors | Controllers
type Vendor         = String

data ComponentPassport = ComponentPassport
  { componentClass  :: ComponentClass
  , componentName   :: ComponentName
  , componentGuid   :: PhysicalGuid
  , componentVendor :: Vendor
  }


data Measurement = Measurement Parameter Float
data Period = Secondly
