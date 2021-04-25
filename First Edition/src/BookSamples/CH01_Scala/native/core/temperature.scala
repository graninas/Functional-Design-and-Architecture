package native.core {

  abstract class temperature

  case class Kelvin(value: Float) extends temperature
  case class Celsius(value: Float) extends temperature

}
