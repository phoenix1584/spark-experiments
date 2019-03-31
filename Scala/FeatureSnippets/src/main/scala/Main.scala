package scalabookcode

import scalabookcode.Classes.{BasicPoint, ClassConstructors, ClassMembers}
import scalabookcode.HelloWorld._
import scalabookcode.Traits.{BasicSubTyping, BasicTrait}

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world from Scala.")
    args.foreach(arg => println(arg))
    for (arg <- args)
      println(arg)

    // Val (immutable) and Var (mutable)
    val testVal = 100
    var testVar = 10 + testVal
    println(testVar, testVal)

    // Sample function.
    val sqr = (x: Int) => x * x
    println(sqr(testVal))

    // Calling a method
    println(add(testVal, testVar))

    // Calling method with param list
    println(addThenMultiply(testVal, testVar)(testVar))
    println(getSquareString(14))

    // Object of a class
    val greeter = new Greeter("Hello ", " !")
    greeter.greet("developer")

    // Case classes
    val p1 = Point(1, 2)
    val p2 = Point(1, 2)
    val p3 = Point(2, 1)
    ComparePoint(p1, p2)
    ComparePoint(p2, p3)

    objectDemo()
    traitDemo()


    BasicPoint()
    ClassConstructors()
    ClassMembers()

    BasicTrait()
    BasicSubTyping()
  }
}
