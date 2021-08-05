module Andromeda.Hardware.Device (
    Device,
    DeviceComponent,
    makeDevice,
    blankDevice,
    addSensor,
    addController,
    getComponent,
    updateComponent
 ) where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Components.API (SensorAPI, ControllerAPI)
import Andromeda.Hardware.Hdl

-- This is a sample of a bad design. The code knows about
-- specific components and manufacturers.
import Andromeda.Vendors.AAA (t25SensorName, p02SensorName
  , t25Handler, p02Handler
  -- , c86ControllerName, c86Handler
  )
--

import Data.Map (Map)
import qualified Data.Map as Map


data DeviceComponent
  = SensorImpl     ComponentDef SensorAPI
  | ControllerImpl ComponentDef ControllerAPI


data Device
  = DeviceImpl (Map ComponentIndex DeviceComponent)



blankDevice :: Device
blankDevice = DeviceImpl Map.empty

makeDevice :: Hdl -> Device
makeDevice hdl = makeDevice' hdl blankDevice
  where
    makeDevice' [] d = d
    makeDevice' (c:cs) d    = makeDevice' cs (add' c d)
    add' (Sensor c idx p)   = addSensor idx p c
    add' (Controller c idx) = addController idx c



-- This is a sample of a bad design. The code knows about
-- specific components and manufacturers.
addSensor :: ComponentIndex -> Parameter
          -> ComponentDef -> Device -> Device
addSensor idx _
  def@(ComponentDef cClass cName cGuid cVendor)
  (DeviceImpl components)
    | cName == t25SensorName = DeviceImpl (add' t25Handler)
    | cName == p02SensorName = DeviceImpl (add' p02Handler)
    | otherwise = error "unknown component"                      -- bad practice
  where
    add' h = Map.insert idx (sensor h) components
    sensor h = SensorImpl def h
--



addController :: ComponentIndex -> ComponentDef
              -> Device -> Device
addController = undefined

getComponent :: ComponentIndex -> Device
             -> Maybe DeviceComponent
getComponent = undefined

updateComponent :: ComponentIndex -> DeviceComponent
                -> Device -> Maybe Device
updateComponent = undefined
