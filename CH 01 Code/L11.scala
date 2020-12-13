import Native.Core._

object Observer {
   def readAndSendTemperature() {
      def toCelsius(data: Native.Core.Temperature) : Float =
        data match {
          case Native.Core.Kelvin(v) => 273.15f - v
          case Native.Core.Celsius(v) => v
        }

      val received = Native.Core.Thermometer.read()
      val inCelsius = toCelsius(received)
      val corrected = inCelsius - 12.5f    // defected device!
      ServerContext.Connection.send("temperature", "T-201A", corrected)
   }
}

object L11 extends App {
  Observer.readAndSendTemperature();
}

object Misc {
	object PureExamples {
		def max(a: Float, b: Float): Float = {
			math.max(a, b)
		}
		
		def calc(a: Int, b: Int, c: Float) : Float = {
			val sum = a + b
			val average = sum / 2
			max(average, c)
		}
	}
	
	object ImpureExamples {
		def launchMissle() {
			println("aa")
		}
		
		def max(a: Float, b: Float): Float = {
			launchMissle()
			math.max(a, b)
		}
		
		def calc(a: Int, b: Int, c: Float) : Float = {
			val sum = a + b
			val average = sum / 2
			max(average, c)
		}
	}
}
