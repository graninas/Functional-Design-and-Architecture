module ScenarioDsl where

import Data.Time

data Value = FloatValue Float
           | IntValue Int
           | StringValue String
  deriving (Show)
           
type Name = String
type Duration = DiffTime
data Status = Online | Offline
data Controller = Controller Name
type Power = Float
type Temperature = Float

type Script = [Procedure]

data Procedure
    = ReadTemperature Controller (Temperature -> Script)
    | Report Value
    | Store Value
    | AskStatus Controller (Status -> Script)
    | InitBoosters (Controller -> Script)
    | HeatUpBoosters Power Duration

reportAndStore :: Value -> Script
reportAndStore val = [ Report val, Store val ]

processTemp :: Temperature -> Script
processTemp t = reportAndStore (temperatureToValue t)

heatingUpScript :: Controller -> Script
heatingUpScript controller =
    [ ReadTemperature controller processTemp
    , HeatUpBoosters 1.0 (seconds 10)
    , ReadTemperature controller processTemp ]

initAndHeatUpScript :: Script
initAndHeatUpScript = [ InitBoosters heatingUpScript ]

-------- Interpreter --------------------------

interpretScript :: Script -> IO ()
interpretScript []     = return ()
interpretScript (p:ps) = do
    interpretProcedure p
    interpretScript ps

interpretProcedure :: Procedure -> IO ()
interpretProcedure (ReadTemperature _ f) = do
    print "Read temperature"
    interpretScript (f 0.0)
interpretProcedure (Report v) = print $ "Report: " ++ show v
interpretProcedure (Store v)  = print $ "Store: "  ++ show v
interpretProcedure (AskStatus _ f) = do
    print "Ask status"
    interpretScript (f Online)
interpretProcedure (InitBoosters f) = do
    print "Init boosters"
    interpretScript (f (Controller "a00"))
interpretProcedure (HeatUpBoosters _ _) = print "Heat up boosters"


-- service functions:
seconds s = secondsToDiffTime s

temperatureToValue :: Temperature -> Value
temperatureToValue t = FloatValue t

