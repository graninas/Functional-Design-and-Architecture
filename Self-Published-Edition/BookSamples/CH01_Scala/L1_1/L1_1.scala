package native.core {

  abstract class Temperature

  case class Kelvin(value: Float) extends Temperature
  case class Celsius(value: Float) extends Temperature

  object Thermometer {
    def getData() = {
      println("Thermometer returned data.")
      Kelvin(100.0f)
    }
  }

  object Utils {
    def toCelsius(data: Float) : Float = data - 273.15f
    def toCelsius(data: native.core.Temperature) : Float =
      data match {
        case native.core.Kelvin(v)  => toCelsius(v)
        case native.core.Celsius(v) => v
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


object Observer {
  def readAndSendTemperature() {

    def toCelsius(data: native.core.Temperature) : Float =
      data match {
        case native.core.Kelvin(v) => 273.15f - v
        case native.core.Celsius(v) => v
      }

    val received = native.core.Thermometer.getData()
    val inCelsius = toCelsius(received)
    val corrected = inCelsius - 12.5f    // defected device!
    server.connection.send("temperature", "T-201A", corrected)
  }
}


object L1_1 {
  def main(args: Array[String]) {
    Observer.readAndSendTemperature();
  }
}
