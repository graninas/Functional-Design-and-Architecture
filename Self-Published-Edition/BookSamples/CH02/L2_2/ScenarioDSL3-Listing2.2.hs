module ScenarioDSL2 where

import Data.Time

type Name = String

type Duration = DiffTime
-- service function:
seconds s = secondsToDiffTime s

data Value = FloatValue Float
           | IntValue Int
           | StringValue String

data Controller = Controller Name

type Power = Float
type Temperature = Float

temperatureToValue :: Temperature -> Value
temperatureToValue t = FloatValue t

type Script = [Procedure]

data Procedure
    = ReadTemperature Controller (Temperature -> Script)
    | Report Value
    | Store Value
    | HeatUpBoosters Power Duration

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
