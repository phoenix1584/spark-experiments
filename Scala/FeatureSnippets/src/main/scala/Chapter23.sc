// For expression revisited
case class Person(name: String, isMale:Boolean,children: Person*)
val lara = Person("lara",false)
val bob = Person("bob",true)
val julie = Person("julie",false,lara,bob)
val persons = List(lara,bob,julie)

persons filter (p => !p.isMale) flatMap(p => (p.children map (c =>(p.name,c.name))))
persons withFilter( p => !p.isMale) flatMap( p=> (p.children map (c=>(p.name,c.name))))
for(p <- persons; if !p.isMale; c <- p.children)
yield (p.name,c.name)

// N queens problem
def queens(n :Int) : List[List[(Int,Int)]] = {
  def placeQueens (k : Int) : List[List[(Int,Int)]] =
    if(k == 0)
      List(List())
    else
      for{
        queens <- placeQueens(k -1)
        column <- 1 to n
        queen = (k, column)
        if isSafe(queen,queens)
      } yield queen :: queens
  def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) =
    queens forall (q => !inCheck(queen,q))

  def inCheck(q1: (Int, Int), q2: (Int, Int)) =
    q1._1 == q2._1 || // same row
    q1._2 == q2._2 || // same column
        (q1._1 - q2._1).abs == (q1._2 - q2._2).abs // on diagonal

  placeQueens(n)
}

//Querying with "for" expressions
case class Book(title: String, authors: String*)

val books: List[Book] =
  List(
    Book("Structure and Interpretation of Computer Programs",
    "Abelson, Harold", "Sussman,Gerald J."),
    Book("Principles of Compiler Design",
        "Aho, Alfred", "Ullman,Jeffery"),
    Book("Programming in Modula-2", "Wirth,Niklaus"),
    Book("Elements of ML Programming","Ullman,Jeffery"),
    Book("The Java Language Specification",
      "Gosling James", "Joy,Bill","Steele,Guy","Bracha,Gilad")
  )

for(b <- books;a <- b.authors
  if a startsWith "Gosling")
  yield b.title

for (b <- books if (b.title indexOf "Program") >= 0) yield b.title
val listDups = for (b1 <- books; b2 <- books if b1 != b2;
     a1 <- b1.authors; a2 <- b2.authors if a1 == a2)
  yield a1

def removeDuplicates[A](xs: List[A]): List[A] = {
  if(xs.isEmpty) xs
  else
    xs.head :: removeDuplicates(
      // xs.tail filter (x => x != xs.head) OR
      for (x <- xs.tail if x != xs.head) yield x
    )
}
removeDuplicates(listDups)

