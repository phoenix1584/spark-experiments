// Implicit conversions
// Implicit conversion to an expected type
implicit def doubleToInt(x : Double) = x.toInt
val i : Int = 3.5

// Converting the receiver
// Implicit classes
case class Rectangle(width:Int,height:Int)
implicit class RectangleMaker(width: Int) {
  def x(height: Int) = Rectangle(width,height)
}
val myRectangle = 3 x 4

class PreferredPrompt(val preference :String)
class PreferredDrink(val preference: String)
object Greeter{
  def greet(name: String)(implicit prompt: PreferredPrompt, drink: PreferredDrink) = {
    println("Welcome " + name + ". The system is ready.")
    println("But while you work, ")
    println("why not enjoy a cup of " + drink.preference + "?")
    println(prompt.preference)
  }
}

val bobsPrompt = new PreferredPrompt("relax> ")
Greeter.greet("Bob")(bobsPrompt,new PreferredDrink("coffee"))

object JoesPrompt{
  implicit val prompt = new PreferredPrompt("Yes, master> ")
  implicit val drink = new PreferredDrink("tea")
}
import JoesPrompt._
Greeter.greet("Joe")

def maxListImpParm[T](elements: List[T])
    (implicit ordering: Ordering[T]): T =
{
  elements match {
    case List() => throw new IllegalArgumentException("empty list!")
    case List(x) => x
    case x :: rest =>
      val maxRest = maxListImpParm(rest)(ordering)
      if(ordering.gt(x,maxRest)) x
      else maxRest
  }
}

maxListImpParm(List(1,2,3))

def maxListImpParm1[T : Ordering](elements: List[T])
{
  elements match {
    case List() => throw new IllegalArgumentException("empty list!")
    case List(x) => x
    case x :: rest =>
      val maxRest = maxListImpParm(rest)
      if(implicitly[Ordering[T]].gt(x,maxRest)) x
      else maxRest
  }
}