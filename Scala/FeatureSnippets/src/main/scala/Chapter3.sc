import java.math.BigInteger

// Lists
val big = new BigInteger("1234444")
val numNames = Array("one","two","three")
val listNums = List(1,2,3,4)
val newListNums = 0 :: listNums
println (newListNums)

newListNums.head
newListNums.init
newListNums(3)
List(100,200) ::: List(300,400)
newListNums.filter(s => s !=3)
newListNums.forall(s => s%5 >= 0)

// Tuples
val t1 = (1, "test_string")
println(t1._1)
println(t1._2)
println(t1)
(1, "test_string",'q')

// Sets and Maps
import scala.collection.mutable

var jetSet = Set("test","string")
var jetSetMutable = mutable.Set("mutable","data")
jetSetMutable += "testData"
jetSet = jetSet + "testData"

val treasureMap = mutable.Map[Int,String]()
treasureMap += (1 -> "Initial data")
treasureMap += (2 -> "Intermediate data")
treasureMap += (2 -> "Overwriting Intermediate data")

def printArgs(args: Array[String]): Unit = {
  args.foreach(println)
}

printArgs(numNames)

def formatArgs(args: Array[String]) = args.mkString("\n")

println(formatArgs(numNames))

val padding = "-" * 10
println(padding)