//Type Parameterization
// Part 1 : Functional Queues
// Initial implementation. Crude in terms of API of constructor.
class Queue1[+T](private val leading:List[T], private val trailing: List[T]){
  private def mirror =
    if(leading.isEmpty)
      new Queue1(trailing.reverse,Nil)
    else
      this

  def head = mirror.leading.head

  def tail = {
    val q = mirror
    new Queue1(q.leading.tail,q.trailing)
  }

  def enqueue[U >: T](x: U) =
    new Queue1[U](leading,x ::trailing )
}

trait Queue[T]{
  def head : T
  def tail : Queue[T]
  def enqueue(x: T): Queue[T]
}

object Queue{
  def apply[T](xs: T*): Queue[T] =
    new QueueImpl[T](xs.toList,Nil)

  private class QueueImpl[T](
      private val leading: List[T],
      private val trailing: List[T]
  ) extends Queue[T]{
    private def mirror =
      if(leading.isEmpty)
        new QueueImpl(trailing.reverse,Nil)
      else
        this

    def head = mirror.leading.head

    def tail = {
      val q = mirror
      new QueueImpl(q.leading.tail,q.trailing)
    }

    def enqueue(x:T) =
      new QueueImpl(leading,x ::trailing )
  }
}

// Variance Annotations
class Cell[T](init: T){
  private[this] var current = init
  def get = current
  def set(x:T) = {current = x}
}

//Covariance and Contravariance of Functions1S
class Publication(val title: String)
class Book(title:String) extends Publication(title)

object Library{
  val books: Set[Book] =
    Set(
      new Book("Programming in Scala."),
      new Book("Walden")
    )
  def printBookList(info: Book => AnyRef) = {
    for(book <- books) println(info(book))
  }
}

object Customer extends App{
  def getTitle(p: Publication) : String = p.title
  Library.printBookList(getTitle)
}

class QueueOpt[+T] private(
    private[this] var leading: List[T],
    private[this] var trailing: List[T]
){
    private def mirror() =
      if(leading.isEmpty){
        while(!trailing.isEmpty) {
          leading = trailing.head :: leading
          trailing = trailing.tail
        }
      }

    def head: T = {
      mirror()
      leading.head
    }

    def tail: QueueOpt[T] ={
      mirror()
      new QueueOpt(leading.tail,trailing)
    }

    def enqueue[U >: T](x: U)=
      new QueueOpt[U](leading, x :: trailing)
}

class Person(val firstName: String,val lastName: String) extends Ordered[Person] {
  override def compare(that: Person): Int = {
    val lastNameComparison = lastName.compareToIgnoreCase(that.lastName)
    if (lastNameComparison != 0)
      lastNameComparison
    else
      firstName.compareToIgnoreCase(that.firstName)
  }

  override def toString: String = firstName + " " + lastName
}

def orderedMergeSort[ T <: Ordered[T]] (xs : List[T]) : List[T] = {
  def merge(xs:List[T], ys: List[T]) : List[T]
    = (xs,ys) match {
    case (Nil,_) => ys
    case (_,Nil) => xs
      case(x :: xs1, y :: ys1) =>
        if(x < y) x :: merge(xs1,ys)
        else y :: merge(xs,ys1)
  }
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (ys,zs) = xs splitAt n
    merge(orderedMergeSort(ys),orderedMergeSort(zs))
  }
}