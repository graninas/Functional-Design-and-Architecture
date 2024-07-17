module Andromeda.TestData.Scripts where

import Andromeda

import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Language.DeviceControl as L
import qualified Andromeda.LogicControl.Language as L

import qualified Data.Map as Map
import Control.Monad.State (StateT, put, get)


createBoosters :: Hdl (Controller, Controller)
createBoosters = do
  lCtrl <- L.setupController lBooster lBoosterController aaaController86Passport
  L.registerComponent lCtrl nozzle1p aaaPressure02Passport
  L.registerComponent lCtrl nozzle1t aaaTemperature25Passport

  rCtrl <- L.setupController rBooster rBoosterController aaaController86Passport
  L.registerComponent rCtrl nozzle2p aaaPressure02Passport
  L.registerComponent rCtrl nozzle2t aaaTemperature25Passport
  pure (lCtrl, rCtrl)


getControllerStatus :: Controller -> LogicControl (Either LogicControlFailure ControllerStatus)
getControllerStatus ctrl = L.getStatus ctrl



createRotaryThruster :: Hdl Controller
createRotaryThruster = do
  ctrl <- L.setupController
    rotaryThruster
    rotaryThrusterController
    aaaController86Passport
  L.registerComponent ctrl nozzle1p aaaPressure02Passport
  L.registerComponent ctrl nozzle1t aaaTemperature25Passport
  pure ctrl


createMainEngine :: Hdl Controller
createMainEngine = do
  ctrl <- L.setupController
    mainEngine
    mainEngineController
    aaaController86Passport
  L.registerComponent ctrl nozzle1p aaaPressure02Passport
  L.registerComponent ctrl nozzle1t aaaTemperature25Passport
  pure ctrl
