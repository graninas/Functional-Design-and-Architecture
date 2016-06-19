module Andromeda.Hardware.Device (
    Device,
    DeviceComponent,
    blankDevice,
    makeDevice,
    addSensor,
    addController,
    getComponent,
    updateComponent,
    setMeasurement,
    readMeasurement
 ) where

import Control.Monad.State.Class
import Control.Monad.Free
import Unsafe.Coerce
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

-- TODO: use lenses!

type Guid = String
data ComponentClass = Sensors | Controllers
data Parameter = Temperature | Pressure

data ComponentDef = ComponentDef
    { componentClass :: ComponentClass
    , componentGuid :: Guid
    , componentManufacturer :: String
    , componentName :: String }
    
type ComponentIndex = String
data Component 
    = Sensor' ComponentDef ComponentIndex Parameter
    | Controller' ComponentDef ComponentIndex

type Hdl = [Component]

sensor = Sensor
controller = Controller

temperature = Temperature
pressure = Pressure

-- Defined somewhere else.
type Measurement = ()
toMeasurement :: Parameter -> Measurement
toMeasurement = undefined

data DeviceComponent = Sensor Measurement Guid
                     | Controller Guid

newtype Device = DeviceImpl (M.Map ComponentIndex DeviceComponent)

blankDevice :: Device
blankDevice = DeviceImpl M.empty


makeDevice :: Hdl -> Device
makeDevice hdl = makeDevice' hdl blankDevice
  where
    makeDevice' [] d = d
    makeDevice' (c:cs) d = makeDevice' cs (add' c d)
    add' (Sensor' c idx p)   = addSensor idx (toMeasurement p) c
    add' (Controller' c idx) = addController idx c

addSensor :: ComponentIndex -> Measurement -> ComponentDef -> Device -> Device
addSensor idx m c (DeviceImpl d) = DeviceImpl $ M.insert idx (Sensor m (componentGuid c)) d

addController :: ComponentIndex -> ComponentDef -> Device -> Device
addController idx c (DeviceImpl d) = DeviceImpl $ M.insert idx (Controller (componentGuid c)) d

getComponent :: ComponentIndex -> Device -> Maybe DeviceComponent
getComponent idx (DeviceImpl d) = undefined

updateComponent :: ComponentIndex -> DeviceComponent -> Device -> Maybe Device
updateComponent idx c d = undefined

setMeasurement :: ComponentIndex -> Measurement -> Device -> Maybe Device
setMeasurement = undefined 

readMeasurement :: ComponentIndex -> Device -> Maybe Measurement
readMeasurement = undefined



