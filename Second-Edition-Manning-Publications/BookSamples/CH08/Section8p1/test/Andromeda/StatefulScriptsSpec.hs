module Andromeda.StatefulScriptsSpec where

import Test.Hspec

import Andromeda

import Andromeda.Assets (createBoosters, aaaController86Name)
import Andromeda.Assets.Vendors.AAA.HardwareService (aaaHardwareService)

import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl
import qualified Andromeda.Hardware.Impl.Interpreters.Hdl as HdlImpl
import qualified Andromeda.Hardware.Impl.Interpreters.DeviceControl as DCImpl

import qualified Andromeda.LogicControl.Impl.Interpreter as LCImpl

import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Language.DeviceControl as L
import qualified Andromeda.LogicControl.Language as L

import qualified Andromeda.TestData.Scripts as Test

import Control.Exception
import Control.Monad.State (StateT, put, get, execStateT, lift)



data SpaceshipProperties = SpaceshipProperties
  { spMass :: Mass
  }

data SpaceshipModel = SpaceshipModel
  { smSpaceshipProperties :: SpaceshipProperties
  , smMainEngine :: Controller
  , smRotationThruster :: Controller
  }

type Burn = Command

type X a = StateT Int IO a

type ShipControl a = StateT SpaceshipModel LogicControl a


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
  -> AngularImpulse
calcAngularImpulse _ _ = AngularImpulse 0

recalcMass :: Mass -> AngularImpulse -> Mass
recalcMass shipMass impulse = shipMass    -- Dummy


performBurn :: Controller -> Burn -> ShipControl ()
performBurn _ _ = pure () -- dummy

wait :: Int -> LogicControl ()
wait _ = pure ()  -- dummy

calcBurn
  :: AngularImpulse
  -> AngularImpulse
  -> Angle
  -> (Burn, Burn, Int)
calcBurn _ _ _ = (undefined, undefined, 0) -- dummy



performRotation :: Angle -> ShipControl ()
performRotation angle = do

  model :: SpaceshipModel <- get

  let thrusterCtrl = smRotationThruster model
  let shipMass1 = spMass $ smSpaceshipProperties model

  eMbTorqueProp <- lift (getProperty thrusterCtrl "torque" [])
  torque        <- lift (validateTorque eMbTorqueProp)

  let impulse1  = calcAngularImpulse shipMass1 torque
  let shipMass2 = recalcMass shipMass1 impulse1
  let impulse2  = calcAngularImpulse shipMass1 torque
  let shipMass3 = recalcMass shipMass2 impulse2
  let (burnStart, burnStop, time) = calcBurn impulse1 impulse2 angle

  performBurn thrusterCtrl burnStart            -- mass changes
  lift $ wait time
  performBurn thrusterCtrl burnStop             -- mass changes




spaceshipRotation :: LogicControl SpaceshipModel
spaceshipRotation = do
  mainEngineCtrl <- L.evalHdl Test.createMainEngine
  thrusterCtrl   <- L.evalHdl Test.createRotaryThruster
  let shipProps  = SpaceshipProperties (Kilogram 100000.0)
  let shipModel1 = SpaceshipModel shipProps mainEngineCtrl thrusterCtrl
  shipModel2 <- execStateT (performRotation (Radian 100.0)) shipModel1
  pure shipModel2



-- TODO: proper tests
spec :: Spec
spec =
  describe "Scripts tests" $ do
    describe "Spaceship rotation script" $ do

      it "Missing vendor components" $ do
        runtime <- RImpl.createHardwareRuntime aaaHardwareService
        eResult <- try $ LCImpl.runLogicControl runtime spaceshipRotation
        case eResult of
          Left (e :: SomeException) -> pure ()
          Right _ -> fail "Unexpected success"
