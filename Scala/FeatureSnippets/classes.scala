object Classes extends App{
    println ("Exploring classes.")
    class User
    val user1 = new User;

    def BasicPoint() : Unit = {
        class Point(var x:Int,var y:Int){
            def move(dx:Int,dy:Int) : Unit = {
                x = x + dx
                y = y + dy
            }

            override def toString: String ={
                s"($x,$y)"
            }
        }

        val point1 = new Point(2,3)
        point1.move(1,1)
        println(point1.toString)
    }

    def ClassConstructors() : Unit = {
        class Point(var x:Int = 0, var y: Int = 0) {
            override def toString : String = {
                s"(x:$x,y:$y)"
            }
        }
        val p1 = new Point
        val p2 = new Point(1)
        val p3 = new Point(y=2)
        println(p1, p2, p3)

    }

    BasicPoint()
    ClassConstructors()
}

Classes.main(args)