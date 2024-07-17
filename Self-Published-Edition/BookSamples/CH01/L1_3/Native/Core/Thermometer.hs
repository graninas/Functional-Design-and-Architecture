module Native.Core.Thermometer where

data Temperature = Kelvin Float
                 | Celsius Float

r = putStrLn "Read"

read :: String -> IO Temperature
read "T-201A"  = (r >>) $ return $ Kelvin 323.5
read "T-35B"   = (r >>) $ return $ Kelvin 325.1
read "T-201AC" = (r >>) $ return $ Kelvin 643.8
read n = error $ "Thermometer not found: " ++ n

