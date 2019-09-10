import com.sun.javafx.scene.layout.region.Margins.Converter

//Abstract members
trait Abstract{
  type T
  def transform(x: T) : T
  val initial: T
  var current: T
}

class Concrete extends Abstract {
  type T = String
  override def transform(x: String): String = x + x
  val initial: String = "hello"
  var current: String = initial
}

trait RationalTrait{
  val numerArg: Int
  val denomArg: Int
  require(denomArg != 0)
  private val g = gcd(numerArg,denomArg)
  val numer = numerArg/g
  val denom = denomArg/g
  private def gcd(a: Int,b: Int): Int =
    if(b==0) a else gcd(b,a % b)
  override def toString = numer + "/" + denom
}

new {
  val numerArg = 1
  val denomArg = 2
} with RationalTrait

object twoThirds extends {
  val numerArg = 2
  val denomArg = 3
} with RationalTrait

object Demo{
  val x = println("Initializing x")
  lazy val lx = println("Initializing l(azy)x")
}

Demo
Demo.lx

trait LazyRationalTrait{
  val numerArg: Int
  val denomArg: Int

  private val g = {
    require(denomArg != 0)
    gcd(numerArg, denomArg)
  }
  lazy val numer = numerArg/g
  lazy val denom = denomArg/g
  private def gcd(a: Int,b: Int): Int =
    if(b==0) a else gcd(b,a % b)
  override def toString = numer + "/" + denom
}

class Food
abstract class Animal{
  type SuitableFood <: Food
  def eat(food: SuitableFood)
}

class Grass extends Food
class Cow extends Animal{
  override type SuitableFood = Grass
  override def eat(food: Grass) = {}
}

class Fish extends Food
val bessy = new Cow
bessy eat (new Grass)
// bessy eat (new Fish) // Does not compile as expected.

// Refinement classes
class Pasture{
  var animals : List[Animal{ type SustainableFood = Grass}] = Nil
}

// Enumerations
object Color extends Enumeration{
  val Red,Green,Blue = Value
}

object Direction extends Enumeration{
  val North = Value("North")
  val East = Value("East")
  val South = Value("South")
  val West = Value("West")
}

for (d <- Direction.values) print(d + " ")

abstract class CurrencyZone{
  type Currency <: AbstractCurrency
  def make(x: Long): Currency
  abstract class AbstractCurrency{
    val amount: Long
    def designation: String
    //override def toString = amount + " " + designation
    override def toString: String = ((amount.toDouble / CurrencyUnit.amount.toDouble )
        formatted ("%." + decimals(CurrencyUnit.amount) + "f")
        + " " + designation)
    def + (that: Currency): Currency = make(this.amount + that.amount)
    def * (x : Double) : Currency = make((this.amount * x).toLong)
    def - (that : Currency) : Currency = make(this.amount - that.amount)
    def / (that: Double) = make((this.amount/that).toLong)
    def / (that: Currency) = this.amount.toDouble / that.amount
    private def decimals(n : Long) : Int =  if(n == 1) 0 else 1 + decimals(n / 10)
    def from(other: CurrencyZone#AbstractCurrency) : Currency =
      make(math.round(
        other.amount.toDouble * Converter.exchangeRate
        (other.designation)(this.designation)))
  }
  val CurrencyUnit: Currency
}

object Converter{
  var exchangeRate = Map(
    "USD" -> Map("USD" -> 1.0, "EUR" -> 0.7596),
    "EUR" -> Map("USD" -> 1.316, "EUR" -> 1.0)
  )
}

object US extends CurrencyZone{
  abstract class Dollar extends AbstractCurrency{
    override def designation: String = "USD"
  }

  type Currency = Dollar
  def make(cents:Long) = new Dollar{val amount = cents}
  val Cent = make(1)
  val Dollar = make(100)
  val CurrencyUnit = Dollar
}

object Europe extends CurrencyZone{
  abstract class Euro extends AbstractCurrency {
    def designation: String = "EUR"
  }

  type Currency = Euro
  def make(cents: Long) = new Euro{
    val amount = cents
  }
  val Cent = make(1)
  val Euro = make(100)
  val CurrencyUnit = Euro
}

US.Dollar from Europe.Euro
US.Dollar + (US.Dollar from Europe.Euro)
// US.Dollar + Europe.Euro // Will Not compile