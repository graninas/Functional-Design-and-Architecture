
module Andromeda.HardwareSpec where

import           Test.Hspec

import Andromeda

import Andromeda.Assets (boostersDef, aaaController86Name)
import Andromeda.Assets.Vendors.AAA.ComponentsAPI (aaaVendorComponents)



-- Drafting the domain (Domain-Driven Design in Haskell)
data LogicMethod = LogicMethod

type Logic = [LogicMethod]


data LogicControlMethod
  = HdlScript Hdl
  | LogicScript Logic


type LogicControl = [LogicControlMethod]


-- monitorTemperature :: DevicePart -> m ()
-- monitorTemperature d = do
--   measurement <- withHandler d readMeasurement
--   t <- validate Temperature measurement
--   if t > 100.0
--     then alarm "Temperature exceeds critical!"
--     else pure ()
--
--
--
-- logicControlScript :: LogicControl ()
-- logicControlScript = do
--   boosters <- makeDevice boostersDef
--   therm <- getDevicePart "nozzle1-t" boosters
--
--   periodically 1.0 $ monitorTemperature therm
--

data LogicMethod = LogicMethod

type Logic = [LogicMethod]

data LogicControlMethod
  = forall a. HdlScript (Hdl a) (a -> LogicControl)
  | LogicScript Logic

type LogicControl = [LogicControlMethod]

logicControlScript :: LogicControl

------------

data Handle = Handle
  { makeDevice :: Hdl -> Device
  , getBlankDeivce :: ...
  }

---------- ---
-- Domain & BL
-- Hdl, Hndl, Scripts, ....

----  --------
-- Impl
-- Device
-- Device interop






spec :: Spec
spec =
  describe "Hardware tests" $ do

    it "Hardware device components check" $ do

      let boosters = makeDevice aaaVendorComponents boostersDef
      let mbThermometer = getDevicePart "nozzle1-t" boosters

      case mbThermometer of
        Nothing -> fail "There is no such component"
        Just thermometer -> putStrLn "Component found."

    it "Hardware device component method run" $ do

      let boosters = makeDevice aaaVendorComponents boostersDef
      let mbThermometer = getDevicePart "nozzle1-t" boosters

      case mbThermometer of
        Nothing -> fail "There is no such component"
        Just thermometer -> do
          withHandler thermometer $ \handler -> do
            measurement <- readMeasurement handler
            measurement `shouldBe` (Measurement Temperature 100.0)
