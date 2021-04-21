{-# LANGUAGE DeriveFunctor #-}
module Andromeda.Hardware.HDL where

import Control.Monad.State.Class
import Control.Monad.Free
import Control.Monad.Trans
import Unsafe.Coerce
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

type Guid = String
data ComponentClass = Sensors | Controllers
data Parameter = Temperature | Pressure

-- Just component-related documentation.
data ComponentDef = ComponentDef
    { componentClass :: ComponentClass
    , componentGuid :: Guid
    , componentManufacturer :: String
    , componentName :: String }
    
type ComponentIndex = String

-- Type to define components of devices.
-- This type is functor due to Haskell's automatic functor instance declaration.
data Component a
    = SensorDef ComponentDef ComponentIndex Parameter a
    | ControllerDef ComponentDef ComponentIndex a
  deriving Functor

-- Free language
type Hdl a = Free Component a

-- Smart constructors
sensor :: ComponentDef -> ComponentIndex -> Parameter -> Hdl ()
sensor c idx p = Free (SensorDef c idx p (Pure ()))

controller :: ComponentDef -> ComponentIndex -> Hdl ()
controller c idx = Free (ControllerDef c idx (Pure ()))

temperature = Temperature
pressure = Pressure

-- Defined somewhere else.
type Measurement = ()
toMeasurement :: Parameter -> Measurement
toMeasurement _ = ()

-- The 'runtime' type of component that we will transform our HDL definition to.
data DeviceComponent = Sensor Measurement Guid
                     | Controller Guid
  deriving (Eq, Show)

-- The 'runtime' type of device that we will transform our HDL definition to.
newtype Device = DeviceImpl (M.Map ComponentIndex DeviceComponent)
  deriving (Eq, Show)

-- Device helper functions
blankDevice :: Device
blankDevice = DeviceImpl M.empty

addSensor :: ComponentIndex -> Parameter -> ComponentDef -> Device -> Device
addSensor idx p c (DeviceImpl d) = DeviceImpl $ M.insert idx (Sensor (toMeasurement p) (componentGuid c)) d

addController :: ComponentIndex -> ComponentDef -> Device -> Device
addController idx c (DeviceImpl d) = DeviceImpl $ M.insert idx (Controller (componentGuid c)) d

-- Interface to the abstract type Device
getComponent :: ComponentIndex -> Device -> Maybe DeviceComponent
getComponent idx (DeviceImpl d) = undefined

updateComponent :: ComponentIndex -> DeviceComponent -> Device -> Maybe Device
updateComponent idx c d = undefined

setMeasurement :: ComponentIndex -> Measurement -> Device -> Maybe Device
setMeasurement = undefined 

readMeasurement :: ComponentIndex -> Device -> Maybe Measurement
readMeasurement = undefined

-- Interpreting - the process aimed to create a device from definition
interpretComponent d (SensorDef c idx par next) = (addSensor idx par c d, next)
interpretComponent d (ControllerDef c idx next) = (addController idx c d, next)

makeDevice' d (Pure _) = d
makeDevice' d (Free comp) =
  let (d1, next) = interpretComponent d comp
  in makeDevice' d1 next
      
makeDevice :: Hdl () -> Device
makeDevice f = makeDevice' blankDevice f

-- Boosters device definition
guid1 = "3539390d-f189-4434-bd9e-d39e494a869a"
guid2 = "c80a1ba3-1e7a-4af9-a10b-2313cc10f9e4"
guid3 = "02ecabc3-1a02-481f-8e9a-7b0cc62e38f5"

aaa_p_02 = ComponentDef Sensors     guid1 "AAA Inc." "AAA-P-02"
aaa_t_25 = ComponentDef Sensors     guid2 "AAA Inc." "AAA-T-25"
aaa_c_86 = ComponentDef Controllers guid3 "AAA Inc." "AAA-C-86"

boostersDef :: Hdl ()
boostersDef = do
   sensor aaa_t_25 "nozzle1-t" temperature
   sensor aaa_p_02 "nozzle1-p" pressure
   sensor aaa_t_25 "nozzle2-t" temperature
   sensor aaa_p_02 "nozzle2-P" pressure
   controller aaa_c_86 "controller"

test = do
    let boostersDevice = makeDevice boostersDef
    print boostersDevice
