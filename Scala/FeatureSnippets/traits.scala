object Traits extends App{
  def BasicTrait() : Unit = {
    trait Iterator[A] {
      def hasNext : Boolean
      def next() : A 
    }
    
    class IntIterator(to: Int) extends Iterator[Int] {
      private var current = 0
      override def hasNext: Boolean = current < to
      override def next(): Int = {
        if(hasNext) {
          val t = current
          current += 1
          t
        } else 0
      }
    }
    val bound = 10
    val iterator = new IntIterator(bound)
    for ( a <- 1 to bound) {
      println(iterator.next)
    }
  }
  
  def BasicSubTyping() : Unit = {
    import scala.collection.mutable.ArrayBuffer

    trait Pet {
      val name : String
    }
    class Cat(val name:String) extends Pet
    class Dog(val name:String) extends Pet

    val dog = new Dog("TestDog")
    val cat = new Cat("TestCat")
    val animals = ArrayBuffer.empty[Pet]
    animals.append(dog)
    animals.append(cat)
    animals.foreach(pet => println(pet.name))

  }

  BasicTrait()
  BasicSubTyping()

}

Traits.main(args)
