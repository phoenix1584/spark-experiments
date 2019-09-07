val diag3 : List[List[Int]] = List ( List(1,0,0),List(0,1,0), List(0,0,1))

val fruits = "apples" :: ("oranges" :: ("pears" :: Nil))
val nums  = 1 :: ( 2 :: ( 3:: Nil))
val nums_1 = 10 :: 20 :: 30 :: 40 :: Nil

nums.head
nums.tail
nums.isEmpty

// Insertion Sort
def isort(xs: List[Int]): List[Int] =
  if (xs.isEmpty) Nil
  else insert(xs.head,isort(xs.tail))

def insert(x: Int, xs: List[Int]): List[Int] =
  if(xs.isEmpty || x <= xs.head) x :: xs
  else xs.head :: insert(x, xs.tail)

val List(a,b,c) = fruits
val x::y::rest = fruits

def isort_1(xs: List[Int]) : List[Int] = xs match {
  case List() => List()
  case x :: xs1 => insert(x,isort_1(xs1))
}

def insert_1(x: Int, xs: List[Int]) : List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x,ys)
}

val ys = List(1,2) ::: List(3,4,5)
ys.init
ys.last
ys take 2
ys drop 2
ys splitAt 2
ys.apply(2)
ys(2)
ys.indices


def append[T](xs : List[T], ys : List[T]) : List[T] =
  xs match {
    case List() => ys
    case x :: xs1 => x :: append(xs1,ys)
  }

fruits.map(_.toCharArray).flatten

val zipped = fruits zip nums
zipped.unzip

fruits mkString ("pre#","-","#post")
fruits mkString "+"

val buf = new StringBuilder

fruits addString (buf,"=")
val farr = fruits.toArray
val fArrToList = farr.toList

val arr2 = new Array[Int](10)
List(1,2,3) copyToArray (arr2,4)
arr2

val itr = arr2.iterator
itr.next

def msort[T](less: (T,T) => Boolean)
    (xs : List[T]) : List[T] = {

  def merge(xs: List[T], ys: List[T]): List[T] =
    (xs,ys) match {
      case (Nil,_) => ys
      case (_,Nil) => xs
      case(x::xs1,y::ys1) =>
        if(less(x,y)) x :: merge(xs1,ys)
        else y :: merge(xs,ys1)
    }

  val n = xs.length/2
  if (n==0) xs
  else {
    val (ys,zs) = xs splitAt n
    merge(msort(less)(ys),msort(less)(zs))
  }
}

List(1,2,3) map (_ * 2)
fruits map (_.toList.reverse.mkString)
fruits map (_.toList)
fruits flatMap (_.toList)

List.range(1,5) flatMap(
    i => List.range(1,i) map (j => (i,j))
)

var sum = 0
nums foreach (sum += _)
nums filter (_%3 == 0)
nums partition (_%2 == 0)

val randList = List (1,2,4,-1,5,5,6)
randList takeWhile (_ > 0)
randList dropWhile (_ > 0)
randList span (_ > 0)

def hasZeroRow (m : List[List[Int]]) =
  m exists (row => row forall (_ == 0))

hasZeroRow(diag3)

def sum(xs : List[Int]): Int = (0 /: xs)(_ + _)
def product(xs  : List[Int]): Int = (1 /: xs)(_*_)
sum(List(1,2,3,5))
product(List(2,3,4,5))

("" /: fruits)( _ + " " + _)

def flattenLeft[T](xss : List[List[T]]) =
  (List[T]() /: xss)(_ ::: _)

def flattenRight[T](xss : List[List[T]]) =
  (xss :\ List[T]()) (_ ::: _)

def reverseLeft[T](xs : List[T]) =
  (List[T]() /: xs){(ys,y) => y :: ys}

randList sortWith ( _ > _)

// Global methods with List
List.range(1,10,3)
List.fill(3)('z')
List.fill(2,3)(1)

List.tabulate(5)(n => n * n)
List.tabulate(5,5)(_ * _)

List.concat(randList,nums)
(List(10,20),List(3,4,5)).zipped.map(_ * _)
(List("abc","de"),List(3,2)).zipped.map(_.length == _)

def msortSwapped[T](xs : List[T])(less: (T,T) => Boolean)
     : List[T] = {

  def merge(xs: List[T], ys: List[T]): List[T] =
    (xs,ys) match {
      case (Nil,_) => ys
      case (_,Nil) => xs
      case(x::xs1,y::ys1) =>
        if(less(x,y)) x :: merge(xs1,ys)
        else y :: merge(xs,ys1)
    }

  val n = xs.length/2
  if (n==0) xs
  else {
    val (ys,zs) = xs splitAt n
    merge(msort(less)(ys),msort(less)(zs))
  }
}

// msort(_ < _)(randList) // Will not compile, need help for type inference of implicit parameters !!
msort[Int](_ < _)(randList)
