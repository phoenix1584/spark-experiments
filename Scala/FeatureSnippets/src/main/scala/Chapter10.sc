object Element{
  abstract class Element{
    def contents: Array[String]
    def height = contents.length
    def width = if (height == 0) 0 else contents(0).length

    def above(that: Element) : Element ={
      /*new ArrayElement(this.contents ++ that.contents)*/
      val this1 = this widen that.width
      val that1 = that widen this.width
      elem(this1.contents ++ that1.contents)
    }


    def beside(that: Element): Element = {
      /* Imperative Style
      val contents = new Array[String](this.contents.length)
       for(i <- 0 until this.contents.length)
         contents(i) = this.contents(i) + that.contents(i)
       new ArrayElement(contents)*/

      /* Implementation before making classes private
        new ArrayElement(
        for(
          (line1,line2) <- this.contents zip that.contents
        ) yield line1 + line2
      )*/

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



val ae = Element.elem(Array("Hello","World"))
ae.width

val e: Element.Element = Element.elem(Array("Hello"))
e.width

val e3: Element.Element = Element.elem('x',2,3)
println(e3.toString)

object Spiral {
  val space = Element.elem(" ")
  val corner = Element.elem("+")

  def spiral(nEdges: Int,direction: Int): Element.Element = {
    if(nEdges == 1)
      Element.elem("+")
    else{
      val sp = spiral(nEdges - 1,(direction + 3) % 4)
      def verticalBar = Element.elem('|',1,sp.height)
      def horizontalBar = Element.elem('-',sp.width,1)
      if(direction == 0)
        (corner beside horizontalBar) above (sp beside space)
      else if(direction == 1)
        (sp above space) beside (corner above verticalBar)
      else if (direction == 2)
        (space beside sp) above (horizontalBar beside corner)
      else
        (verticalBar above corner) beside (space above sp)
    }
  }
}

val nSides = 10
println(Spiral.spiral(nSides,0))