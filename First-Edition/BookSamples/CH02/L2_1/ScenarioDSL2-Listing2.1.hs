module ScenarioDSL1 where

type Name = String

data Value = FloatValue Float
           | IntValue Int
           | StringValue String

data Controller = Controller Name

type Power = Float
type Temperature = Float

type Script = [Procedure]

data Procedure
    = ReadTemperature Controller
    | Report Value
    | Store Value
    | StartBoosters Power
    | StopBoosters
    | Wait Int

heatUp :: Script
heatUp =
    [ ReadTemperature (Controller "boosters")
    , Report (FloatValue undefined)
    , Store (FloatValue undefined)
    , StartBoosters 1.0
    , Wait 10
    , StopBoosters
    , Report (FloatValue undefined)
    , Store (FloatValue undefined)
    ]
