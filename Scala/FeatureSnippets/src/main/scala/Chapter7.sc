import java.io.{File, FileNotFoundException, FileReader, IOException}

import scala.util.Try

for ( i <- 1 to 4)
  println("iteration" + i)

for (x <- 10 until 13)
  println("itr" + x)

val filesHere = (new File("/home/schiwate/CodeBase/MyGit/spark-experiments/Scala/FeatureSnippets/src/main/scala")).listFiles
def fileLines(file: java.io.File) = scala.io.Source.fromFile(file).getLines().toList

def grep(pattern : String ) = {
  for (file <- filesHere
       if file.isFile
       if file.getName.endsWith(".sc");
       line <- fileLines(file);
       trimmed = line.trim
       if trimmed.matches(pattern)
  ) println(file + ": " + trimmed)
}

grep(".*val.*")

val forLineLengths =
  for {
    file <- filesHere
    if file.getName.endsWith("*.sc");
    line <- fileLines(file)
    trimmed = line.trim
    if trimmed.matches(".*for.*")
  } yield trimmed.length


try{
  val f = new FileReader("input.txt")
} catch {
  case ex: FileNotFoundException => println("File not found.")
  case ex: IOException => println("IO Exception.")
} finally{
  println("Cannot close f here !!!")
}

def makeRowSeq(row: Int) = for (col <- 1 to 10) yield {
  val prod = (row * col).toString
  val padding = " "  * (4 - prod.length)
  padding + prod
}

def makeRow(row:Int) = makeRowSeq(row).mkString

def multiTable = {
  val tableSeq = for (row <- 1 to 10)
    yield makeRow(row)
  tableSeq.mkString("\n")
}

multiTable