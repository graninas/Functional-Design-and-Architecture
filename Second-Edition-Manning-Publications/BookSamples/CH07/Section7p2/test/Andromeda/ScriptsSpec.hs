module Andromeda.ScriptsSpec where

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


data SpaceshipProperties = SpaceshipProperties
  { spaceShipMass :: Mass
  }

validateTorque
  :: Either LogicControlFailure (Maybe Property)
  -> Either LogicControlFailure Torque
validateTorque (Left err) = Left err
validateTorque (Right (Just (PhysicalUnitProperty _ (UnitTorque (Torque t)))))
  = Right (Torque t)
validateTorque err = Left $ LogicControlFailure $ "Torque isn't correct: " <> show err

calculateAngularImpulse
  :: SpaceshipProperties
  -> Torque
  -> TimeInterval
  -> AngularImpulse
calculateAngularImpulse _ _ _ = AngularImpulse 0


getAngularImpulse
  :: Controller
  -> ComponentIndex
  -> SpaceshipProperties
  -> TimeInterval
  -> LogicControl (Either LogicControlFailure AngularImpulse)
getAngularImpulse ctrl idx shipProps interval = do
  eMbTorqueProp <- getProperty ctrl "torque" [ComponentIndexParam idx]
  pure $ case validateTorque eMbTorqueProp of
    Left err -> Left err
    Right torque -> Right $ calculateAngularImpulse shipProps torque interval


spec :: Spec
spec =
  describe "Scripts tests" $ do
    describe "Calculate impulse script" $ do

      it "No torque providing component found" $ do
        runtime <- RImpl.createHardwareRuntime aaaHardwareService

        let script = do
              ctrl <- evalHdl $
                setupController lBooster lBoosterController aaaController86Passport
              let shipProps = SpaceshipProperties (Kilogram 100000)
              let interval = Seconds 10
              getAngularImpulse ctrl (ComponentIndex "1") shipProps interval

        eResult <- LCImpl.runLogicControl runtime script
        case eResult of
          Left e -> pure ()
          Right _ -> fail "Unexpected success"
