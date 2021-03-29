import qualified Native.Core.Thermometer as T
import qualified ServerContext.Connection as C

readThermometer :: String -> IO T.Temperature
readThermometer = T.read

type Seconds = Int

calculateAverage :: [Float] -> Float
calculateAverage values = ...

observeTemperatureDuring :: Seconds -> IO [Float]
observeTemperatureDuring secs = ...
   
getAverageTemperature :: IO Float
getAverageTemperature = let
    values <- observeTemperatureDuring 60
    return $ calculateAverage values

