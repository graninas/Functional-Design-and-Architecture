module Andromeda.Hardware.Common where



-- Every physical instance of a component has its own GUID
type PhysicalGuid = String

-- Components are grouped by name,
-- for example, temperature sensors AAA-T-25
type ComponentName = String

data Parameter      = Temperature | Pressure
  deriving (Show, Eq, Ord)

data ComponentClass = Sensors | Controllers
  deriving (Show, Eq, Ord)

type Manufacturer   = String

data ComponentDef = ComponentDef
  { componentClass        :: ComponentClass
  , componentName         :: ComponentName
  , componentGuid         :: PhysicalGuid
  , componentManufacturer :: Manufacturer
  }
  deriving (Show, Eq, Ord)

data Measurement = Measurement Parameter Float
  deriving (Show, Eq, Ord)

data Period = Secondly
  deriving (Show, Eq, Ord)
