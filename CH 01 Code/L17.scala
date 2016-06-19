package native.light {

    trait ILampSwitcher {
        def switch(onOff: Boolean)
    }
    
    class DaylightLamp (n: String, v: Int, onOff: Boolean)
           extends ILampSwitcher {
        var isOn: Boolean = onOff
        var value: Int = v
        val name: String = n
        def switch(onOff: Boolean) {
            isOn = onOff
            println("Lamp " + name + " switched: " + isOn)
        }
    }
    
    class TableLamp (n: String, onOff: Boolean)
            extends ILampSwitcher {
        var isOn: Boolean = onOff
        val name: String = n
        def switch(onOff: Boolean) = {
            isOn = onOff
            println("Lamp " + name + " switched: " + isOn)
            // Debug: will remove it later!
            throw new Exception("switched")
        }
    }
}

object L17 {

    var d1 = new native.light.DaylightLamp("G1", 100, false)
    var d2 = new native.light.DaylightLamp("G2", 100, false)
    var t1 = new native.light.TableLamp("T1", false)

    def turnAllOff(lamps: List[native.light.ILampSwitcher]) {
        lamps.foreach(_.switch(false))
    }

    def main(args: Array[String]) {
        var lamps = List(d1, d2, t1)
    
        turnAllOff(lamps)
    }
}
