package server {
    object connection {
        def send(name: String, dataType: String, v: Float) = {
            println("Sended: ")
            println(v)
        }
    }
}

trait ISensor {
    def getData() : Float
    def getName() : String
    def getDataType() : String
}
trait IConnection {
    def send(name: String, dataType: String, v: Float)
}

final class Receiver extends IConnection {
    def send(name: String, dataType: String, v: Float) =
        server.connection.send(name, dataType, v)
}

final class Thermometer extends ISensor {
    val correction = -12.5f
    def transform(data: native.core.Temperature) : Float =
        native.core.utils.toCelsius(data) + correction

    def getName() : String = "T-201A"
    def getDataType() : String = "temperature"
    def getData() : Float = {
        val data = native.core.thermometer.getData()
        transform(data)
    }
}

final class Observer (s: ISensor, c: IConnection) {
    val sensor: ISensor = s
    val connection: IConnection = c

    def readAndSendData() {
        val data = sensor.getData()
        val sensorName = sensor.getName()
        val dataType = sensor.getDataType()
        connection.send(sensorName, dataType, data)
    }
}

object L11 {

    def observeThermometerData() {
        val t = new Thermometer()
        val r = new Receiver()
        val observer = new Observer(t, r)
        observer.readAndSendData()
    }

    def main(args: Array[String]) {
        observeThermometerData()
    }
}
