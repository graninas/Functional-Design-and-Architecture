module Andromeda.Hardware.Common where

import Andromeda.Common.Physics

-- Every physical instance of a component has its own GUID
type PhysicalGuid = String

-- Components are grouped by name,
-- for example, temperature sensors AAA-T-25
type ComponentName = String

data SensorType
  = TemperatureSensor
  | PressureSensor
  deriving (Show, Eq, Ord)

data ComponentClass
  = Sensors SensorType
  | Controllers
  deriving (Show, Eq, Ord)

newtype SensorMeasurement = SensorMeasurement PhysicalUnit
  deriving (Show, Eq, Ord)

type Vendor = String

data ComponentPassport = ComponentPassport
  { componentClass  :: ComponentClass
  , componentName   :: ComponentName      -- Supposed to be unique on the market
  , componentGuid   :: PhysicalGuid       -- Supposed to be unique on the market
  , componentVendor :: Vendor
  }
  deriving (Show, Eq, Ord)
