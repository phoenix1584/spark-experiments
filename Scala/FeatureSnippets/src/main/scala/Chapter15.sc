sealed abstract class Expr
case class Var(name : String) extends Expr
case class Number(num : Double) extends Expr
case class UnOp (operator: String, arg: Expr) extends Expr
case class BinOp(operator: String,left : Expr, right: Expr) extends Expr

val v = Var("x")
val op = BinOp("+",Number(1),v)
op.copy(operator = "*")

def SimplifyTop(expr: Expr): Expr = expr match{
  case UnOp("-", UnOp("-",e)) => e
  case BinOp("+",e,Number(0)) => e
  case BinOp("*",e,Number(1)) => e
  case _ => expr
}

SimplifyTop(UnOp("-",UnOp("-",Var("y"))))

def JustCompare(expr: Expr): Unit = expr match{
  case BinOp(_,_,Number(0)) => println("deep match...") // Watch the order!!
  case BinOp(_,_,_) => println(expr + "is a binary expression.")
  case _ => println("Hmmm... something else")
}

val binOpInstance = BinOp("/",Number(1), Number(5))
JustCompare(binOpInstance)
JustCompare(v)
JustCompare(BinOp("/",Number(1), Number(0)))

def isStringArray(x: Any) = x match {
  case a : Array[String] => "yes"
  case _ => "no"
}

val as = Array("abc")

def varMatch (expr: Expr) = expr match{
  case UnOp("abs", e @ UnOp("abs", _)) => e
  case _ =>
}

def simplifyAdd(e: Expr): Expr = e match{
  case BinOp("+",x,y) if x == y =>
    BinOp("*",x,Number(2))
  case _ => e
}

simplifyAdd(BinOp("+",v,v))

def describe(e : Expr) = (e: @unchecked) match{
  case Var(_) => "a variable"
  case Number(_) => "a number"
  // case _ => throw new RuntimeException // Not ideal
}

def show(x : Option[String]) = x match {
  case Some(s) => s
  case None => "?"
}

val capitals = Map("France" -> "Paris","USA" -> "Washington DC","Japan" -> "Tokyo")

show (capitals get "North Pole")

val myTuple = (123,"123")
val(num,str) = myTuple

// val second : List[Int] => Int = { // Not Exhaustive
val second : PartialFunction[List[Int],Int] = {
  case x :: y :: _ => y
}

second.isDefinedAt(List(5,6,7))
second.isDefinedAt(List())

for ((country, city) <- capitals)
  println("The capital of " + country + " is " + city)

object Element{
  abstract class Element{
    def contents: Array[String]
    def height = contents.length
    def width = if (height == 0) 0 else contents(0).length

    def above(that: Element) : Element ={
      val this1 = this widen that.width
      val that1 = that widen this.width
      elem(this1.contents ++ that1.contents)
    }


    def beside(that: Element): Element = {
      val this1 = this heighten that.height
      val that1 = that heighten this.height
      elem(
        for((line1,line2) <- this1.contents zip that1.contents)
          yield line1 + line2)
    }

    def widen(w: Int): Element =
      if (w <= width) this
      else{
        val left = elem(' ',(width - 2)/2,height)
        val right = elem(' ',w - width - left.width, height)
        left beside this beside right
      }

    def heighten(h: Int): Element =
      if(h <= height) this
      else{
        val top = elem(' ', width, (h - height)/2)
        val bot = elem(' ',width,h - height - top.height)
        top above this above bot
      }

    override def toString = contents mkString "\n"
  }

  class ArrayElement(conts: Array[String]) extends Element {
    override def contents: Array[String] = conts
  }

  class TerseArrayElements(val contents : Array[String]) extends Element

  class Line(s: String) extends Element{
    val contents = Array(s)
    override def width: Int = s.length
    override def height: Int = 1
  }

  class UniformElement(
      ch: Char,
      override val width: Int,
      override val height: Int
  ) extends Element {
    private def line = ch.toString * width
    def contents = Array.fill(height)(line)
  }

  def elem(contents: Array[String]): Element = new ArrayElement(contents)

  def elem(chr: Char, width: Int, height: Int): Element =
    new UniformElement(chr,width,height)

  def elem(s: String): Element =
    new Line(s)
}

class ExprFormatter{

  // Contains operators in groups of increasing precedence
  private val opGroups = Array(
    Set("|","||"),
    Set("&","&&"),
    Set("^"),
    Set("==","!="),
    Set("<","<=",">",">="),
    Set("+","-"),
    Set("*","%")
  )

  // Mapping from operators to their precedence
  private val precedence = {
    val assocs = for {
      i <- 0 until opGroups.length
      op <- opGroups(i)
    } yield op -> i
    assocs.toMap
  }

  private val unaryPrecedence = opGroups.length
  private val fractionPrecedence = -1

  private def format(e:Expr, enclPrec: Int) : Element.Element = {
    e match {
      case Var(name) => Element.elem(name)
      case Number(num) =>
        def stripDots(s: String) =
          if (s endsWith ".0") s.substring(0,s.length - 2)
          else s
        Element.elem(stripDots(num.toString))

      case UnOp(op, arg) =>
        Element.elem(op) beside format(arg,unaryPrecedence)

      case BinOp("/",left,right) =>
        val top = format(left,fractionPrecedence)
        val bottom = format(right,fractionPrecedence)
        val line = Element.elem('-',top.width max bottom.width,1)
        val frac = top above line above bottom
        if(enclPrec != fractionPrecedence) frac
        else Element.elem(" ") beside frac

      case BinOp(op,left,right) =>
        val opPrec = precedence(op)
        val l = format(left,opPrec)
        val r = format(right, opPrec + 1)
        val oper = l beside Element.elem(" " + op + " " ) beside r
        if (enclPrec <= opPrec) oper
        else Element.elem("(") beside oper beside Element.elem(")")
    }
  }

  def format(e: Expr) : Element.Element = format(e,0)
}

val f = new ExprFormatter

val e1 = BinOp("*",BinOp("/",Number(1),Number(2)),BinOp("+",Var("x"),Number(1)))
val e2 = BinOp("+",BinOp("/",Var("x"), Number(2)),BinOp("/",Number(1.5),Var("x")))
val e3 = BinOp("/",e1,e2)

def show(e: Expr) = println(f.format(e) + "\n\n")

for (e <- Array(e1,e2,e3)) show(e)