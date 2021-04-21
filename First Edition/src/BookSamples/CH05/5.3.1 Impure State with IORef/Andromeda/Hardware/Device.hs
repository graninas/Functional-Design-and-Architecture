module Andromeda.Hardware.Device where

import Andromeda.Hardware.HDL
import Andromeda.Hardware.Types

import qualified Data.Map as M

data DeviceComponent = Sensor Measurement Guid
                     | Controller Guid
  deriving (Read, Show, Eq)

newtype Device = DeviceImpl (M.Map ComponentIndex DeviceComponent)
  deriving (Read, Show, Eq)
  
blankDevice :: Device
blankDevice = DeviceImpl M.empty

addSensor :: ComponentIndex -> Parameter -> ComponentDef -> Device -> Device
addSensor idx p c (DeviceImpl d) = DeviceImpl $ M.insert idx (Sensor (toMeasurement p) (componentGuid c)) d

addController :: ComponentIndex -> ComponentDef -> Device -> Device
addController idx c (DeviceImpl d) = DeviceImpl $ M.insert idx (Controller (componentGuid c)) d

getComponent :: ComponentIndex -> Device -> Maybe DeviceComponent
getComponent idx (DeviceImpl d) = error "getComponent"

updateComponent :: ComponentIndex -> DeviceComponent -> Device -> Maybe Device
updateComponent idx c d = error "updateComponent"

setMeasurement :: ComponentIndex -> Measurement -> Device -> Maybe Device
setMeasurement = error "setMeasurement" 

readMeasurement :: ComponentIndex -> Device -> Maybe Measurement
readMeasurement = error "readMeasurement"

    
