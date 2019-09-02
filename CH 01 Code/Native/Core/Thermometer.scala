package Native.Core {
	
	sealed trait Temperature
	case class Kelvin(value: Float) extends Temperature
	case class Celsius(value: Float) extends Temperature
	
	object Thermometer {
		def read(): Temperature = {
			println("Thermometer returned data.")
			Kelvin(100.0f)
		}
	}
	
	object HighAccuracyThermometer {
		def read() = {
			println("HA Thermometer returned data.")
			100.0001f
		}
	}
	
	object Utils {
		def toCelsius(data: Float) : Float = data - 273.15f
		def toCelsius(data: Native.Core.Temperature) : Float =
			data match {
				case Native.Core.Kelvin(v)  => toCelsius(v)
				case Native.Core.Celsius(v) => v
			}
	}
}


