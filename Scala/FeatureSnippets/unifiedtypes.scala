
object UnifiedType extends App{
    println("Exploring unified types.")

    val list: List[Any] = List (
        "string", //string
        1, // Int
        2.0, // double
        true, // boolean
        () => "anon function returing a string."
    )

    list.foreach(elem => println(elem))
}

UnifiedType.main(args)