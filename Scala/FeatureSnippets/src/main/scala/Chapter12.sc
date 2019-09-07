trait Philosophical{
  def philosophize() = {
    println("I consume memory,therefore I am!!")
  }
}

class Animal

class Frog extends Animal with Philosophical{
  override def toString = "green"
  override def philosophize() ={
    println("It ain't easy being " + toString + "!")
  }
}

val frog = new Frog
frog.philosophize()

class Rational(n: Int, d: Int) extends Ordered[Rational]{
  require(d!=0)
  val numer = n
  val denom = d
  def compare (that: Rational) ={
    (this.numer * that.denom) - (this.denom * that.numer)
  }
}

abstract class IntQueue{
  def get() : Int
  def put(x : Int)
}

import scala.collection.mutable.ArrayBuffer

class BasicIntQueue extends IntQueue{
  private val buf = new ArrayBuffer[Int]
  def get = buf.remove(0)
  def put(x:Int) = { buf += x}
}

val queue = new BasicIntQueue

queue.put(10)
queue.put(11)
queue.put(12)

queue.get()

trait Doubling extends IntQueue{
  abstract override def put(x: Int): Unit = super.put( 2 * x)
}

class MyQueue extends BasicIntQueue with Doubling

val myQueue = new MyQueue
myQueue.put(10)
myQueue.get()

trait Incrementing extends IntQueue {
  abstract override def put(x: Int): Unit = super.put(x + 1)
}

trait Filtering extends IntQueue {
  abstract override def put(x: Int): Unit = if(x >= 0 ) super.put(x)
}

val myNewQueue = (new BasicIntQueue with Incrementing with Filtering)
myNewQueue.put(-1)
myNewQueue.put(0)
myNewQueue.put(1)
myNewQueue.get()