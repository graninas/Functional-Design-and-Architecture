object L17 extends App {
	import L12.{ISensor, IConnection, Observer}
	import L15.{Receiver}
	
	final class HighAccuracyThermometer {
		def name() : String = "HAT-53-2"
		def getKelvin() : Float = {
			Native.Core.HighAccuracyThermometer.read()
		}
	}
	
	final class HAThermometerAdapter(val thermometer: HighAccuracyThermometer) extends ISensor {
		def getData() : Float = {
			val data = thermometer.getKelvin()
			Native.Core.Utils.toCelsius(data)
		}
		def getName() : String = thermometer.name()
		def getDataType() : String = "temperature"
	}
	
	object Worker {
		def observeThermometerData() {
			val hatMismatched = new HighAccuracyThermometer()
			val hatAdapted = new HAThermometerAdapter(hatMismatched)
			val r = new Receiver()
			val observer = new Observer(hatAdapted, r)
			observer.readAndSendData()
		}
	}
	
	Worker.observeThermometerData()
}
