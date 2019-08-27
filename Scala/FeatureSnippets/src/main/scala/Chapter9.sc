object FileMatcher{
  private def filesHere = (new java.io.File(".")).listFiles

  private def fileMatching(matcher: String => Boolean) =
    for (file <- filesHere; if matcher(file.getName))
      yield file

  def filesEnding(query:String) =
    fileMatching(_.endsWith(query))

  def filesContaining(query:String) =
    fileMatching(_.contains(query))

  def filesRegex(query:String) ={
    fileMatching(_.matches(query))
  }
}

def containsNeg(nums:List[Int])= nums.exists(_ < 0)

def containsOdd(nums:List[Int]) = nums.exists(_ % 2 == 1)

containsOdd(List(1,2,3,4,5))

// Currying

def curriedSum(x: Int)(y: Int) = x + y
curriedSum(10)(20)

val onePlus = curriedSum(1)_
onePlus(2)

def twice(op: Double => Double,x: Double) = op(op(x))
twice(_ + 2 , 5)

var assertionEnabled = true

def myAssert(predicate: () => Boolean) =
  if(assertionEnabled && !predicate())
    throw new AssertionError

myAssert(() => 5 > 3)

def byNameAssert(predicate: => Boolean) =
  if(assertionEnabled && !predicate)
    throw new AssertionError

byNameAssert(5 > 3)

