module Main where


type Time = Float

type ThrustDx = Float
type Name = String

newtype SolidFuelBoosters = SolidFuelBoosters Name
newtype MainEngine = MainEngine Name
newtype RotaryEngine = RotaryEngine Name

data FlightProcedure
  = Delay Time
  | InitSolidFuelBoosters SolidFuelBoosters
  | InitMainEngine MainEngine
  | InitRotaryEngine RotaryEngine
  | DecoupleSolidFuelBoosters SolidFuelBoosters
  | ChangeMainEngineThrust MainEngine ThrustDx Time
  | ChangeRotaryEngineThrust RotaryEngine ThrustDx Time

type FlightControl = [FlightProcedure]

boosters :: SolidFuelBoosters
boosters = SolidFuelBoosters "solid fuel boosters"

rotaryEngine :: RotaryEngine
rotaryEngine = RotaryEngine "rotary engine"

mainEngine :: MainEngine
mainEngine = MainEngine "main engine"


rocketOrbitalLaunch :: FlightControl
rocketOrbitalLaunch =
  -- Launch
  [ InitSolidFuelBoosters boosters
  , InitMainEngine mainEngine
  , InitRotaryEngine rotaryEngine

  -- Lift off, first 30 seconds of launch
  , ChangeMainEngineThrust mainEngine 100.0 (seconds 30)
  , Delay (seconds 30)

  -- Powered ascent
  , ChangeRotaryEngineThrust rotaryEngine 10.0 (seconds 20)
  , Delay (seconds 20)

  -- Staging
  , DecoupleSolidFuelBoosters boosters
  , ChangeRotaryEngineThrust rotaryEngine 20.0 (seconds 20)
  , Delay (seconds 20)

  -- Upper stage burn
  , ChangeRotaryEngineThrust rotaryEngine 30.0 (seconds 20)
  , Delay (seconds 20)
  ]

--
-- interpretFlightProcedure :: FlightProcedure -> IO ()
-- interpretFlightProcedure (Delay t) = delay t
-- interpretFlightProcedure (InitSolidFuelBoosters name) = ...
-- interpretFlightProcedure ...     -- other procedures
--
-- runFlightControl :: FlightControl -> IO ()
-- runFlightControl [] = pure ()
-- runFlightControl (p:ps) = do
--   interpretFlightProcedure p
--   runFlightControl ps


seconds x = x
delay x = pure ()


main :: IO ()
main = pure ()
