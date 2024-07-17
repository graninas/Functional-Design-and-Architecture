module ScenarioDSL4 where

import Data.Time

type Name = String

type Duration = DiffTime
-- service function:
seconds s = secondsToDiffTime s

data Value = FloatValue Float
           | IntValue Int
           | StringValue String
  deriving (Show, Read)
  
data Controller = Controller Name
  deriving (Show, Read)
  
type Temperature = Float
type Power = Float
type Script = [Procedure]

data Status = Online | Offline
   deriving (Eq, Show, Read)

data Procedure
    = ReadTemperature Controller (Temperature -> Script)
    | Report Value
    | Store Value
    | AskStatus Controller (Status -> Script)
    | InitBoosters (Controller -> Script)
    | HeatUpBoosters Power Duration


temperatureToValue :: Temperature -> Value
temperatureToValue t = FloatValue t

heatUp :: Controller -> Script
heatUp controller = [
    ReadTemperature controller (\t1 ->
        [ Report (temperatureToValue t1)
        , Store  (temperatureToValue t1)
        , HeatUpBoosters 1.0 (seconds 10)
        , ReadTemperature controller (\t2 ->
            [ Report (temperatureToValue t2)
            , Store  (temperatureToValue t2) ])
        ])
    ]

tryHeatUp :: Controller -> Script
tryHeatUp controller = [
    AskStatus controller (\status ->
        if (status == Online)
        then heatUp controller
        else []
    )]

boostersHeatingUp :: Script
boostersHeatingUp =
    [ InitBoosters tryHeatUp
    , Report (StringValue "Script finished.") ]

someController = Controller "a00"

scriptInterpreter :: Script -> IO ()
scriptInterpreter []     = return ()
scriptInterpreter (p:ps) = do
    interpret p
    scriptInterpreter ps

interpret :: Procedure -> IO ()
interpret (ReadTemperature _ f) = do
    print "Read temperature"
    scriptInterpreter (f 0.0)
interpret (Report v) = print $ "Report: " ++ show v
interpret (Store v)  = print $ "Store: "  ++ show v
interpret (AskStatus _ f) = do
    print "Ask status"
    scriptInterpreter (f Online)
interpret (InitBoosters f) = do
    print "Init boosters"
    scriptInterpreter (f (Controller "a00"))
interpret (HeatUpBoosters _ _) = print "Heat up boosters"



mock :: Procedure -> IO ()
mock (ReadTemperature _ f) = do
    print "Read temperature"
    eval (f 0.0)
mock (Report _) = print "Report value"
mock (Store _)  = print "Store value"
mock (AskStatus _ f) = do
    print "Ask status"
    eval (f Online)
mock (InitBoosters f) = do
    print "Init boosters"
    eval (f (Controller "a00"))
mock (HeatUpBoosters _ _) = print "Heat up boosters"


main = interpretScript boostersHeatingUp
