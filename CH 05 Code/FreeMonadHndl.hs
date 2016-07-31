{-# LANGUAGE DeriveFunctor #-}
module Andromeda.Hardware.HNDL where

import Control.Monad.State.Class
import Control.Monad.Free
import Control.Monad.Trans
import Unsafe.Coerce
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

-- General types defined somewhere else.

type Guid = String
type Measurement = ()
toMeasurement :: Parameter -> Measurement
toMeasurement _ = ()

-- ------------------- Hardware definition language - HDL. -------------------------------------------------------

-- Just component-related documentation.
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

class HdlInterpreter m where
   onSensorDef :: Monad m => ComponentDef -> ComponentIndex -> Parameter -> m ()
   onControllerDef :: Monad m => ComponentDef -> ComponentIndex -> m ()
   
interpretHdl :: (Monad m, HdlInterpreter m) => Hdl a -> m a
interpretHdl (Pure a) = return a
interpretHdl (Free proc) = case proc of
    SensorDef cd idx par next -> do
        onSensorDef cd idx par
        interpretHdl next
    ControllerDef cd idx next -> do
        onControllerDef cd idx
        interpretHdl next
        
-- Smart constructors
sensor :: ComponentDef -> ComponentIndex -> Parameter -> Hdl ()
sensor c idx p = Free (SensorDef c idx p (Pure ()))

controller :: ComponentDef -> ComponentIndex -> Hdl ()
controller c idx = Free (ControllerDef c idx (Pure ()))

temperature = Temperature
pressure = Pressure

-- ----------------------------------- Hardware network definition language - HNDL -----------------------------------------------------

type DeviceIndex = (DeviceObjectIndex, ComponentIndex)

data DeviceInterface = DeviceInterface DeviceIndex
  deriving (Show, Eq)
data TerminalUnitInterface = TerminalUnitInterface PhysicalAddress
  deriving (Show, Eq)
newtype Interface = Interface PhysicalAddress
  deriving (Show, Eq)

-- | Convenient language for defining devices in network.
data HndlItem a = DeviceDef (Hdl DeviceIndex) Description (DeviceInterface -> a)
                | TerminalUnitDef PhysicalAddress Description (TerminalUnitInterface -> a)
                | LogicControlDef PhysicalAddress Description (Interface -> a) -- TODO: seems unuseful. Duplicates TerminalUnitDef.
                | LinkedDeviceDef RemoteDeviceInterface TerminalUnitInterface a
                | LinkDef Interface TerminalUnitInterface a
  deriving (Functor)

-- | Free monad Hardware Network Definition Language.
-- It's just a definition, but not a real hardware network.
type Hndl a = Free HndlItem a

class HndlInterpreter m where
   onRemoteDeviceDef :: Monad m => Hdl DeviceIndex -> Description -> m RemoteDeviceInterface
   onTerminalUnitDef :: Monad m => PhysicalAddress -> Description -> m TerminalUnitInterface
   onLogicControlDef :: Monad m => PhysicalAddress -> Description -> m Interface
   onLinkedDeviceDef :: Monad m => RemoteDeviceInterface -> TerminalUnitInterface -> m ()
   onLinkDef         :: Monad m => Interface -> TerminalUnitInterface -> m ()
   
mkInterface = Interface
mkRemoteDeviceInterface = RemoteDeviceInterface
mkTerminalUnitInterface = TerminalUnitInterface

interpretHndl :: (Monad m, HndlInterpreter m) => Hndl a -> m a
interpretHndl (Pure a) = return a
interpretHndl (Free proc) = case proc of
    RemoteDeviceDef hdl d next -> do
        i <- onRemoteDeviceDef hdl d
        interpretHndl $ next i
    TerminalUnitDef pa d next -> do
        i <- onTerminalUnitDef pa d
        interpretHndl $ next i
    LogicControlDef pa d next -> do
        i <- onLogicControlDef pa d
        interpretHndl $ next i
    LinkedDeviceDef rdi tui next -> do
        onLinkedDeviceDef rdi tui
        interpretHndl $ next
    LinkDef i tui next -> do
        onLinkDef i tui
        interpretHndl $ next






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
