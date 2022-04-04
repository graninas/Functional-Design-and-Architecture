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

import Control.Exception

spaceshipRotation :: LogicControl (Either String ())
spaceshipRotation = do
  let shipProps = Test.SpaceshipProperties (Kilogram 100000.0)
  mainEngineCtrl <- L.evalHdl Test.createMainEngine
  thrusterCtrl   <- L.evalHdl Test.createRotaryThruster
  let shipModel1 = Test.SpaceshipModel shipProps mainEngineCtrl thrusterCtrl
  shipModel2 <- Test.performRotation shipModel1 (Radian 100.0)
  pure $ Right ()


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
