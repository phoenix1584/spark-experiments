import scala.collection.mutable
//Collections in Depth
Traversable(1, 2, 3)
Iterable("x", "y", "z")

//Trait Traversable
val xs = List(1, 2, 3) map (_ + 1)
xs.view


//Trait Iterable
val xs1 = List(1,2,3,4,5)
val grpIt = xs1 grouped 2
grpIt.next
grpIt.next

val slIt = xs1 sliding 3
slIt.next
slIt.next

xs1.iterator
xs1 takeRight 3
xs1 dropRight 2

// Trait Sequence

// Trait Set
val s = Set(1,1,1,3,4,5)
s(2)

// Streams
import scala.collection.immutable.Stream
val str = 1 #:: 2 #:: 3 #:: Stream.empty
def fibForm(a : Int , b: Int) : Stream[Int] =
{
  a #:: fibForm(b,a+b)
}

val fibs = fibForm(1,1).take(10)
fibs.toList

// Vectors
val vec = scala.collection.immutable.Vector.empty
val vec2 = vec :+ 1 :+2
val vec3 = 100 +: vec2
val vec4 = vec3 updated (1,20)

collection.immutable.IndexedSeq(1,2,3)

// red black trees
val set = scala.collection.immutable.TreeSet.empty[Int]
set + 1 + 1 + 2 + 3 + 2

// bit sets
val bits = scala.collection.immutable.BitSet.empty
val morebit = bits + 3 + 4 + 4
morebit(3)
morebit(0)

//Class Tag and generic arrays
import scala.reflect.ClassTag
def evenElems[T : ClassTag](xs: Vector[T]) : Array[T] = {
  val arr = new Array[T]((xs.length + 1) / 2)
  for(i <- 0 until xs.length by 2)
    arr(i/2) = xs(i)
  arr
}

// Views
val v = Vector( 1 to 10 : _*)
val vv = v.view
val vv1 = vv map (_ + 1)
val vv2 = vv1 map (_ * 2)
vv2.force