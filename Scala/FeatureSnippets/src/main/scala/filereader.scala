import scala.io.Source

object filereader {
  def widthOfLengths (s : String) = s.length.toString.length
  def main(args: Array[String]): Unit = {
    if(args.length > 0 ){
      val lines = Source.fromFile(args(0)).getLines().toList
      val longestLine = lines.reduceLeft(
        (a,b) => if (a.length > b.length) a else b
      )
      val maxWidth = widthOfLengths(longestLine)

      for (line <- lines){
        val numSpaces = maxWidth - widthOfLengths(line)
        val padding = " " * numSpaces
        println(padding + line.length + "|" + line)
      }

    }else
        Console.err.print("Please enter a filename")
  }
}
