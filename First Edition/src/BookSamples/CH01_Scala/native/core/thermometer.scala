package native.core {

  object thermometer {
    def getData() = {
      println("Thermometer returned data.")
      Kelvin(100.0f)
    }
  }

}
