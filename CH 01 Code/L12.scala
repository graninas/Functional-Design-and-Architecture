import Native.Core.Temperature

object L12 {
	trait ISensor {
		def getData(): Float
		def getName(): String
		def getDataType(): String
	}
	
	trait IConnection {
		def send(name: String, dataType: String, v: Float)
	}
	
	final class Observer(val sensor: ISensor, val connection: IConnection) {
		def readAndSendData() {
			val data = sensor.getData()
			val sensorName = sensor.getName()
			val dataType = sensor.getDataType()
			connection.send(sensorName, dataType, data)
		}
	}
}

object L15 extends App {
	import L12._
	
	final class Receiver extends IConnection {
		def send(name: String, dataType: String, v: Float) {
			ServerContext.Connection.send(name, dataType, v)
		}
	}
	
	final class Thermometer extends ISensor {
		val correction = -12.5f
		def transform(data: Native.Core.Temperature) : Float =
			Native.Core.Utils.toCelsius(data) + correction
		
		def getName() : String = "T-201A"
		def getDataType() : String = "temperature"
		def getData() : Float = {
			val data = Native.Core.Thermometer.read()
			transform(data)
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
	
	Worker.observeThermometerData()
}
