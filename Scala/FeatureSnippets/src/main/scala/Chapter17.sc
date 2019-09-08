// Sequences
// Lists => covered in Ch 16
// Arrays => More in ch3 sec. 7 and ch10
val arr = new Array[Int](5)
println(arr(1))

// List Buffers
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
val buf = new ListBuffer[Int]
buf += 1
buf += 2
3 +=: buf
buf.toList

//Array Buffers
import scala.collection.mutable.ArrayBuffer
val arrBuf = new ArrayBuffer[Int]()
arrBuf += 12
15 +=: arrBuf

// Strings
def hasUpperCase (s: String) : Boolean = s.exists(_.isUpper)
hasUpperCase("Yes")
hasUpperCase("no")

// Sets and Maps
val mutaSet = mutable.Set(1,2,3)
val text = "See Spot run. Run Spot. Run!"
val wordsArray = text.split("[!,.]+")
val words = mutable.Set.empty[String]
for (word <- wordsArray)
  words += word.toLowerCase

val nums = Set(1,2,3)
nums + 5
nums - 3
nums ++ List(5,6)
nums -- List(1,2)
nums & Set(4,5,6,7) // Takes intersection
nums.size
nums.contains(5)

val wds = mutable.Set.empty[String]
wds += "This"
wds += " is "
wds -= " is "
wds ++= List("is", "a")
wds --= List("This","is")

// maps
val map = scala.collection.mutable.Map.empty[String,Int]
map += ("string" -> 1)
//map("hello") = 2 // Error:(150, 58) ')' expected but string literal found.
//inst$A$A.`get$$instance_0$$map("hello")`; println("map("hello"): " + {val $$temp$$ = inst$A$A.map("hello") ; MacroPrinter.printDefInfo($$temp$$).replace("inst$A$A.", "") + " = " + ( print$$$Worksheet$$$Array$$$($$temp$$) ).stripPrefix("A$A0$A$A0$")})

def countWords(text: String) ={
  val counts = mutable.Map.empty[String,Int]
  for( rawWord <- text.split("[ ,!.]+")){
    val word = rawWord.toLowerCase
    val oldCount = if (counts.contains(word)) counts(word)
    else 0
    counts += (word -> (oldCount + 1))
  }
  counts
}

countWords(text)

val numsMap = Map("i"-> 1,"x" -> 2)
numsMap + ("vi" -> 3)
numsMap - "x"
numsMap ++ List("ii"->2, "jj"->4)
numsMap -- List("ii","i")
numsMap.size
numsMap.contains("ii")
numsMap.keys
numsMap.keySet
numsMap.values
numsMap.isEmpty
map += ("test"->3)
map -= "test"
map ++= List("i"->1,"ii"->2,"iii"->3)
map --= List("i","ii")

// TreeSet, TreeMap
import scala.collection.immutable.{TreeSet,TreeMap}
val ts = TreeSet(9,5,4,3,7,8,1)
val tm = TreeMap("a"->1,"x"->4,"f"->3)

// Initializing collections
val anySet = mutable.Set[Any](42)
anySet += "test"

