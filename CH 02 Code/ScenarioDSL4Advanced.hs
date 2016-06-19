module ScenarioDSL4Advanced where

import Data.Time

type Name = String
type Duration = DiffTime
-- service function:
seconds s = secondsToDiffTime s

data Value = FloatValue Float
           | IntValue Int
           | StringValue String
   deriving (Show, Read, Eq)
              
data Controller = Controller Name
   deriving (Show, Read, Eq)
   
type Power = Float

data Status = Online | Offline
   deriving (Show, Read, Eq)

data Parameter = Temperature | Pressure
   deriving (Show, Read, Eq)
   
data Procedure
    = Script [Procedure]
    | Read Parameter Controller (Value -> Procedure)
    | Report Value
    | Store Value
    | AskStatus Controller (Status -> Procedure)
    | InitBoosters (Controller -> Procedure)
    | HeatUpBoosters Power Duration

heatUpBoosters :: Controller -> Procedure
heatUpBoosters controller = 
    Read Temperature controller (\t1 ->
        Script
            [ Report t1
            , Store  t1
            , HeatUpBoosters 1.0 (seconds 10)
            , Read Temperature controller (\t2 ->
                Script [ Report t2
                       , Store  t2 ])
            ])

tryHeatUp :: Controller -> Procedure
tryHeatUp controller = 
    AskStatus controller (\status ->
        if (status == Online)
        then heatUpBoosters controller
        else Script []
    )


boostersHeatingUp :: Procedure
boostersHeatingUp = Script
    [ InitBoosters tryHeatUp
    , Report (StringValue "Script finished.") ]

someController = Controller "a00"

interpretMany :: [Procedure] -> IO ()
interpretMany []     = return ()
interpretMany (p:ps) = do
    interpret p
    interpretMany ps

interpret :: Procedure -> IO ()
--interpret (Script ps) = interpretMany ps
interpret (Script []) = return ()
interpret (Script (p:ps)) = do
    interpret p
    interpret (Script ps)
interpret (Read Temperature _ f) = do
    print "Read temperature"
    interpret (f $ FloatValue 0.0)
interpret (Read p _ f) = error $ "Not implemented: " ++ show p
interpret (Report v) = print $ "Report: " ++ show v
interpret (Store v)  = print $ "Store: "  ++ show v
interpret (AskStatus _ f) = do
    print "Ask status"
    interpret (f Online)
interpret (InitBoosters f) = do
    print "Init boosters"
    interpret (f (Controller "a00"))
interpret (HeatUpBoosters _ _) = print "Heat up boosters"

main = interpret boostersHeatingUp





    
