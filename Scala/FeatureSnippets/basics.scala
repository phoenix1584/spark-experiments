object HelloWorld{
    // A Method 
    def add (x:Int,y:Int):Int  = x+ y

    // Method with parameter list
    def addThenMultiply(x:Int,y:Int)(multiplier:Int) = add(x,y) * multiplier

    def main(args: Array[String]):  Unit = {
        println("Hello world from Scala.")

        // Val (immutable) and Var (mutable)
        val testVal = 100
        var testVar = 10 + testVal
        println(testVar,testVal)

        // Sample function.
        val sqr = (x:Int) => x * x
        println(sqr(testVal))

        // Calling a method
        println(add(testVal,testVar))

        // Calling method with param list
        println(addThenMultiply(testVal,testVar)(testVar))
        
    }
}