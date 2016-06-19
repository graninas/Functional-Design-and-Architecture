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

object Worker {
    def observeThermometerData() {
        val t = new Thermometer()
        val r = new Receiver()
        val observer = new Observer(t, r)
        observer.readAndSendData()
    }
}

object L14 {

    def main(args: Array[String]) {
        Worker.observeThermometerData()        
    }
}
