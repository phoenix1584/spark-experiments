package scalabookcode
object HelloWorld{
    // A Method 
    def add (x:Int,y:Int):Int  = x+ y

    // Method with parameter list
    def addThenMultiply(x:Int,y:Int)(multiplier:Int) = add(x,y) * multiplier

    // Multiline method syntax 
    def getSquareString (input:Double): String = {
        val square = input * input
        square.toString
    }
    
    // Class
    class Greeter(prefix:String, suffix:String) {
        def greet(name:String): Unit = println(prefix + name + suffix)
    }

    // Case Class : Immutable and are compared by value.
    case class Point(x: Int,y: Int)

    def ComparePoint(x:Point, y:Point) : Unit = {
        if(x == y){
            println(x + " and " + y + " are equal." )
        }else{
            println(x + " and " + y + " are not equal." )
        }
    }

    // Objects : Singletons
    def objectDemo() : Unit = {
        object IDFactory {
            private var counter = 0
            def create(): Int = {
                counter += 1
                counter
            }
        }
        val newID:Int = IDFactory.create()
        println(newID)
        val newerID:Int = IDFactory.create()
        println(newerID)
    }

    // Traits : Traits are types containing certain fields and methods. Multiple traits can be combined.
    def traitDemo() : Unit = {
        trait GreeterT{
            def greet(sal:String ) : Unit = {
                println(sal + " scala programmer !")
            }
        }

        class DefaultGreeter extends GreeterT
        val def_greeter = new DefaultGreeter
        def_greeter.greet("Hello")

        class ModGreeter(custom:String) extends GreeterT{
            override def greet(sal:String) : Unit = {
                println(sal + " " + custom + " scala programmer !")
            }
        }

        val mod_greeter = new ModGreeter("amateur")
        mod_greeter.greet("Hello")
    }
    
}