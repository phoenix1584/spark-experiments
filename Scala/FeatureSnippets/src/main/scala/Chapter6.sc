import com.sun.javafx.tk.ScreenConfigurationAccessor

class Rational(n: Int, d: Int){
  require(d!=0)

  private def gcd(a: Int, b: Int) : Int ={
    if (b ==0 ) a else gcd(b, a%b)
  }

  private val g = gcd(n.abs,d.abs)

  val numer = n/g
  val denom = d/g

  def this(n : Int) = this(n,1)

  override def toString: String = n + "/" + d

  def add(that: Rational) : Rational = {
    return new Rational( numer * that.denom + that.numer * denom, denom * that.denom)
  }

  def lessThan(that: Rational) = (numer * that.denom) < (that.numer * denom)

  def max (that: Rational) = if(this.lessThan(that)) that else this

  def + (that : Rational) : Rational = {
    return new Rational( numer * that.denom + that.numer * denom, denom * that.denom)
  }

  def + ( i : Int) : Rational = {
    return new Rational( numer + i * denom, denom)
  }

  def * (that : Rational) : Rational = {
    return new Rational(numer * that.numer , denom * that.denom)
  }

  def * (i : Int) : Rational ={
    new Rational( numer * i, denom)
  }

  def - (that : Rational) : Rational = {
    return new Rational( numer * that.denom - that.numer * denom, denom * that.denom)
  }

  def - (i : Int) : Rational = {
    return new Rational(numer - i * denom, denom)
  }

  def / (that: Rational) : Rational = {
    return new Rational( numer * that.denom,denom * that.numer)
  }

  def / (i: Int) : Rational = new Rational(numer, denom * i)
}

val r12 = new Rational(1,2)
val r23 = new Rational(2,3)
val rAdd = r12 add r23
val rLT = r12 lessThan r23
val maxRat = r12 max r23
val addFunc = r12 + r23

r12 * 2
r12 / 2

implicit def intToRational(x : Int) : Rational = new Rational(x)
val t : Rational = 2 * r12