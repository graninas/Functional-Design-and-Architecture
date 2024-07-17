module Andromeda.Hardware.HDL where

import Control.Monad.Free

type Guid = String
data ComponentClass = Sensors | Controllers
  deriving (Read, Show)
data Parameter = Temperature | Pressure
  deriving (Read, Show)

data ComponentDef = ComponentDef
    { componentClass :: ComponentClass
    , componentGuid :: Guid
    , componentManufacturer :: String
    , componentName :: String }
  deriving (Read, Show)
    
type ComponentIndex = String

data Component a
    = SensorDef ComponentDef ComponentIndex Parameter a
    | ControllerDef ComponentDef ComponentIndex a
  deriving (Read, Show)

type Hdl a = Free Component a

instance Functor Component where
    fmap f (SensorDef cd idx p a)   = SensorDef cd idx p (f a)
    fmap f (ControllerDef cd idx a) = ControllerDef cd idx (f a)

sensor :: ComponentDef -> ComponentIndex -> Parameter -> Hdl ()
sensor c idx p = Free (SensorDef c idx p (Pure ()))

controller :: ComponentDef -> ComponentIndex -> Hdl ()
controller c idx = Free (ControllerDef c idx (Pure ()))

temperature = Temperature
pressure = Pressure

