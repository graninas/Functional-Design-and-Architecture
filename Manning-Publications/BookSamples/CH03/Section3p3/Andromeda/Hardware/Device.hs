module Andromeda.Hardware.Device (
    Device,
    DevicePart,
    makeDevice,
    blankDevice,
    addSensor,        -- These functions violate encapsulation
    addController,    -- Device internal structure should not be mutable.
    getDevicePart,
    updateDevicePart
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


data DevicePart
  = SensorImpl     ComponentPassport SensorAPI
  | ControllerImpl ComponentPassport ControllerAPI


type DeviceParts = Map ComponentIndex DevicePart

data Device = DeviceImpl DeviceParts



blankDevice :: Device
blankDevice = DeviceImpl Map.empty

makeDevice :: Hdl -> Device
makeDevice hdl = makeDevice' hdl blankDevice
  where
    -- Traversing the list of components (definitions)
    makeDevice' [] device = device
    makeDevice' (c:cs) device = makeDevice' cs (add' c device)
    -- Creating a specific device part (implementation)
    -- by its definition and adding into the Device type
add' (Sensor c idx p)   = addSensor idx p c
add' (Controller c idx) = addController idx c



-- This is a sample of a bad design. The code knows about
-- specific components and manufacturers.
addSensor :: ComponentIndex -> Parameter
          -> ComponentPassport -> Device -> Device
addSensor
  idx _
  def@(ComponentPassport _ cName _ _)
  (DeviceImpl components) = let
      handler = getHandler cName
      sensor  = SensorImpl def handler
      components' = Map.insert idx sensor components
      in DeviceImpl components'

getHandler :: ComponentName -> SensorAPI
getHandler cName | cName == t25SensorName = t25Handler
getHandler cName | cName == p02SensorName = p02Handler
getHandler _     | otherwise = error "unknown component"      -- bad practice


addController :: ComponentIndex -> ComponentPassport
              -> Device -> Device
addController = undefined

getDevicePart :: ComponentIndex -> Device
             -> Maybe DevicePart
getDevicePart = undefined

updateDevicePart :: ComponentIndex -> DevicePart
                -> Device -> Maybe Device
updateDevicePart = undefined
