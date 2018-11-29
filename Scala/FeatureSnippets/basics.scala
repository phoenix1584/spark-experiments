object HelloWorld{
    def main(args: Array[String]):  Unit = {
        println("Hello world from Scala.")

        val testVal = 100
        var testVar = 10 + testVal
        println(testVar,testVal)
    }
}