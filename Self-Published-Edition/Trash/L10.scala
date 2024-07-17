







object L10 {
    def main(args: Array[String]) {

        Observer.readAndSendTemperature();
    }

    def launchMissle() = println("aa")

    def max(a: Float, b: Float) = {
        launchMissle()
        math.max(a, b)
    }

    def calc(a: Int, b: Int, c: Float) : Float =
    {
    val sum = a + b
    val average = sum / 2
    max(average, c)
}
}
