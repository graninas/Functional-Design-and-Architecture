package native.core {

    abstract class Temperature
        
    case class Kelvin(value: Float) extends Temperature
    case class Celsius(value: Float) extends Temperature

    object utils {
      def toCelsius(data: Float) : Float = data - 273.15f
      def toCelsius(data: native.core.Temperature) : Float =
          data match {
              case native.core.Kelvin(v)  => toCelsius(v)
              case native.core.Celsius(v) => v
          }
    }

    object thermometer {
        def getData() = {
            println("Thermometer returned data.")
            Kelvin(100.0f)
        }
    }
    
    object highAccuracyThermometer {
        def getData() = {
            println("HA Thermometer returned data.")
            100.0001f
        }
    }
}

package server {
    object connection {
        def send(name: String, dataType: String, v: Float) = {
            println("Sended: ")
            println(v)
        }
    }
}

trait ISensor {
    def getData() : Float
    def getName() : String
    def getDataType() : String
}

trait IConnection {
    def send(name: String, dataType: String, v: Float)
}

final class HighAccuracyThermometer {
    def name() : String = "HAT-53-2"
    def getKelvin() : Float = {
        native.core.highAccuracyThermometer.getData()
    }
}

final class HAThermometerAdapter
       (thermometer: HighAccuracyThermometer)
       extends ISensor {
    val t = thermometer
    
    def getData() : Float = {
        val data = t.getKelvin()
        native.core.utils.toCelsius(data)
    }
    def getName() : String = t.name()
    def getDataType() : String = "temperature"    
}

final class Receiver extends IConnection {
    def send(name: String, dataType: String, v: Float) =
        server.connection.send(name, dataType, v)
}

final class Thermometer extends ISensor {
    val correction = -12.5f
    def transform(data: native.core.Temperature) : Float =
        native.core.utils.toCelsius(data) + correction

    def getName() : String = "T-201A"
    def getDataType() : String = "temperature"
    def getData() : Float = {
        val data = native.core.thermometer.getData()
        transform(data)
    }
}

final class Observer (s: ISensor, c: IConnection) {
    val sensor: ISensor = s
    val connection: IConnection = c

    def readAndSendData() {
        val data = sensor.getData()
        val sensorName = sensor.getName()
        val dataType = sensor.getDataType()
        connection.send(sensorName, dataType, data)
    }
}

object L16 {

    def observeThermometerData() {
        val hatMismatched = new HighAccuracyThermometer()
        val hatAdapted = new HAThermometerAdapter(hatMismatched)
        val r = new Receiver()
        val observer = new Observer(hatAdapted, r)
        observer.readAndSendData()
    }

    def main(args: Array[String]) {
        observeThermometerData()        
    }
}
