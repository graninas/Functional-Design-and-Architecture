module Andromeda.TestData.Scripts where

import Andromeda

import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Language.DeviceControl as L
import qualified Andromeda.LogicControl.Language as L

import qualified Data.Map as Map


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


data SpaceshipProperties = SpaceshipProperties
  { spMass :: Mass
  }

data SpaceshipModel = SpaceshipModel
  { smSpaceshipProperties :: SpaceshipProperties
  , smMainEngine :: Controller
  , smRotationThruster :: Controller
  }

-- Unsafe function. Yes, I know
validateTorque
  :: Either LogicControlFailure (Maybe Property)
  -> LogicControl Torque
validateTorque (Right (Just (PhysicalUnitProperty _ (UnitTorque (Torque t)))))
  = pure $ Torque t
validateTorque (Left err) = error $ show err

-- Dummy function. Doesn't calculate anything.
calcAngularImpulse
  :: Mass
  -> Torque
  -> LogicControl AngularImpulse
calcAngularImpulse _ _ = pure $ AngularImpulse 0

recalcMass :: Mass -> AngularImpulse -> LogicControl Mass
recalcMass shipMass impulse = pure shipMass    -- Dummy

performImpulse :: Controller -> AngularImpulse -> LogicControl ()
performImpulse _ _ = pure () -- Dummy

negateImpulse :: AngularImpulse -> AngularImpulse
negateImpulse i = i   -- Dummy

getThrusterAngularImpulse
  :: Controller
  -> Mass
  -> LogicControl AngularImpulse
getThrusterAngularImpulse ctrl mass = do
  eMbTorqueProp <- getProperty ctrl "torque" []
  torque <- validateTorque eMbTorqueProp
  calcAngularImpulse mass torque

type Burn = Command


performBurn :: Controller -> Burn -> LogicControl ()
performBurn _ _ = pure () -- dummy

wait :: Int -> LogicControl ()
wait _ = pure ()  -- dummy

calcBurn
  :: AngularImpulse
  -> AngularImpulse
  -> Angle
  -> LogicControl (Burn, Burn, Int)
calcBurn _ _ _ = pure (undefined, undefined, 0) -- dummy

performRotation
  :: SpaceshipModel
  -> Angle
  -> LogicControl SpaceshipModel
performRotation model angle = do
  let thrusterCtrl = smRotationThruster model
  let shipMass1 = spMass $ smSpaceshipProperties model

  impulse1  <- getThrusterAngularImpulse thrusterCtrl shipMass1
  shipMass2 <- recalcMass shipMass1 impulse1
  impulse2  <- getThrusterAngularImpulse thrusterCtrl shipMass2
  shipMass3 <- recalcMass shipMass2 impulse2

  (burnStart, burnStop, time) <- calcBurn impulse1 impulse2 angle

  performBurn thrusterCtrl burnStart
  wait time
  performBurn thrusterCtrl burnStop

  pure $ model { smSpaceshipProperties = SpaceshipProperties shipMass3 }
