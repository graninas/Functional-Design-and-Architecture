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

data ComponentDef = ComponentDef
    { componentClass :: ComponentClass
    , componentGuid :: Guid
    , componentManufacturer :: String
    , componentName :: String }
    
type ComponentIndex = String

data Component a
    = SensorDef ComponentDef ComponentIndex Parameter a
    | ControllerDef ComponentDef ComponentIndex a
  deriving Functor

type Hdl a = Free Component a

sensor :: ComponentDef -> ComponentIndex -> Parameter -> Hdl ()
sensor c idx p = Free (SensorDef c idx p (Pure ()))

controller :: ComponentDef -> ComponentIndex -> Hdl ()
controller c idx = Free (ControllerDef c idx (Pure ()))


data ComponentF a
    = SensorDefF ComponentDef ComponentIndex Parameter a
    | ControllerDefF ComponentDef ComponentIndex a

type HdlF a = Free ComponentF a

instance Functor ComponentF where
    fmap f (SensorDefF cd idx p a)   = SensorDefF cd idx p (f a)
    fmap f (ControllerDefF cd idx a) = ControllerDefF cd idx (f a)

sensorF :: ComponentDef -> ComponentIndex -> Parameter -> HdlF ()
sensorF c idx p = liftF (SensorDefF c idx p ())

controllerF :: ComponentDef -> ComponentIndex -> HdlF ()
controllerF c idx = liftF (ControllerDefF c idx ())

temperature = Temperature
pressure = Pressure

-- Defined somewhere else.
type Measurement = ()
toMeasurement :: Parameter -> Measurement
toMeasurement _ = ()

data DeviceComponent = Sensor Measurement Guid
                     | Controller Guid
  deriving Eq

newtype Device = DeviceImpl (M.Map ComponentIndex DeviceComponent)
  deriving Eq
  
blankDevice :: Device
blankDevice = DeviceImpl M.empty

addSensor :: ComponentIndex -> Parameter -> ComponentDef -> Device -> Device
addSensor idx p c (DeviceImpl d) = DeviceImpl $ M.insert idx (Sensor (toMeasurement p) (componentGuid c)) d

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

makeDevice :: Hdl () -> Device
makeDevice f = makeDevice' blankDevice f
  where
    makeDevice' d (Pure _) = d
    makeDevice' d (Free proc) =
      let (next, d1) = case proc of
            SensorDef c idx par n -> (n, addSensor idx par c d)
            ControllerDef c idx n -> (n, addController idx c d)
      in makeDevice' d1 next


makeDeviceF :: HdlF () -> Device
makeDeviceF f = makeDevice' blankDevice f
  where
    makeDevice' d (Pure _) = d
    makeDevice' d (Free proc) =
      let (next, d1) = case proc of
            SensorDefF c idx par n -> (n, addSensor idx par c d)
            ControllerDefF c idx n -> (n, addController idx c d)
      in makeDevice' d1 next


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

boostersDefF :: HdlF ()
boostersDefF = do
   sensorF aaa_t_25 "nozzle1-t" temperature
   sensorF aaa_p_02 "nozzle1-p" pressure
   sensorF aaa_t_25 "nozzle2-t" temperature
   sensorF aaa_p_02 "nozzle2-P" pressure
   controllerF aaa_c_86 "controller"

boostersDef' :: Hdl ()
boostersDef' = let
    proc1 = Free (SensorDef aaa_t_25 "nozzle1-t" temperature proc2)
    proc2 = Free (SensorDef aaa_p_02 "nozzle1-p" pressure proc3)
    proc3 = Free (SensorDef aaa_t_25 "nozzle2-t" temperature proc4)
    proc4 = Free (SensorDef aaa_p_02 "nozzle2-P" pressure proc5)
    proc5 = Free (ControllerDef aaa_c_86 "controller" endOfChain)
    endOfChain = Pure ()
    in proc1

definition1 = undefined
definition2 = definition1
definition3 = definition2
par1 = undefined
par2 = par1
index1 = undefined
index2 = index1
index3 = index1

a1 :: Component ()
a2 :: Component (Component ())
a3 :: Component (Component (Component ()))
a1 = ControllerDef definition1 index1 ()
a2 = SensorDef definition2 index2 par1 a1
a3 = SensorDef definition3 index3 par2 a2


fa1, fa2, fa3 :: Free Component ()
fa1 = Free (ControllerDef definition1 index1 (Pure ())) 
fa2 = Free (SensorDef definition2 index2 par1 fa1) 
fa3 = Free (SensorDef definition3 index3 par2 fa2)

test = do
    let bi = makeDevice boostersDef
    let bif = makeDeviceF boostersDefF
    print (bi == bif)
    print "Ok."
