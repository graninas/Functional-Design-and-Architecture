package native.core {

  object utils {
    def toCelsius(data: Float) : Float = data - 273.15f
    def toCelsius(data: native.core.temperature) : Float =
      data match {
        case native.core.Kelvin(v)  => toCelsius(v)
        case native.core.Celsius(v) => v
      }
  }

}
