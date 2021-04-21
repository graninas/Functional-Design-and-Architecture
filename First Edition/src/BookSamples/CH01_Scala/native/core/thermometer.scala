package native.core {

  abstract class Temperature

  case class Kelvin(value: Float) extends Temperature
  case class Celsius(value: Float) extends Temperature

  object thermometer {
    def getData() = {
      println("Thermometer returned data.")
      Kelvin(100.0f)
    }
  }

}
