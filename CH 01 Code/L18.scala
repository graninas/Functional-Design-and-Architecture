object L18 extends App {

  trait ILampSwitcher {
    def switch(onOff: Boolean)
  }
  
  final class DaylightLamp(
	  val name: String,
	  val value: Int,
	  var isOn: Boolean
  ) extends ILampSwitcher {
    def switch(onOff: Boolean) {
        isOn = onOff
        println("Lamp " + name + " switched: " + isOn)
    }
  }
  
  class TableLamp(
	  val name: String,
	  var isOn: Boolean
  ) extends ILampSwitcher {
    def switch(onOff: Boolean) {
      isOn = onOff
      println("Lamp " + name + " switched: " + isOn)
      // Debug: will remove it later!
      throw new Exception("switched")
    }
  }
	
	var d1 = new DaylightLamp("G1", 100, false)
	var d2 = new DaylightLamp("G2", 100, false)
	var t1 = new TableLamp("T1", false)
	
	def turnAllOff(lamps: List[ILampSwitcher]) {
		lamps.foreach(_.switch(false))
	}
	
	var lamps = List(d1, d2, t1)
	turnAllOff(lamps)
}