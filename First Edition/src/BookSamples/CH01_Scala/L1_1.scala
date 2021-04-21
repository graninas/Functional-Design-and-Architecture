import native.core.temperature
import native.core.termometer
import native.core.utils

object Observer {
  def readAndSendTemperature() {

    def toCelsius(data: native.core.Temperature) : Float =
      data match {
        case native.core.Kelvin(v) => 273.15f - v
        case native.core.Celsius(v) => v
      }

    val received = native.core.thermometer.getData()
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
