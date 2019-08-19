class Rational(n: Int, d: Int){
  require(d!=0)

  private def gcd(a: Int, b: Int) : Int ={
    if (b ==0 ) a else gcd(b, b%a)
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

  def * (that : Rational) : Rational = {
    return new Rational(numer * that.numer , denom * that.denom)
  }
}

val r12 = new Rational(1,2)
val r23 = new Rational(2,3)
val rAdd = r12 add r23
val rLT = r12 lessThan r23
val maxRat = r12 max r23