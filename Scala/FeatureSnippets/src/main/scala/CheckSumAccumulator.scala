import scala.collection.mutable

class CheckSumAccumulator{
  private var sum = 0;

  def add (b : Byte) : Unit   = sum += b
  def checkSum() : Int = ~(sum & 0xFF) + 1

}

object CheckSumAccumulator {
  private val cache = mutable.Map.empty[String,Int]

  def calculate(s : String) : Int ={
    if (cache.contains(s))
      cache(s)
    else {
      val acc =  new CheckSumAccumulator
      for (c <- s)
        acc.add(c.toByte)
      val cs = acc.checkSum()
      cache += (s -> cs)
      cs
    }
  }
}
