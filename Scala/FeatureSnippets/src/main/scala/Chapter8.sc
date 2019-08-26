var increase = (x: Int) => {
    println("Test line")
    x + 1
}

increase(100)

val nums = List.range(1,10)
nums.foreach((x : Int) => println(x))
nums.filter((x) => x%2 ==0)
nums.filter(_%3 ==0)

val f = (_:Int) + (_:Int)
f(10,60)

def sum( a: Int,b: Int, c:Int) = a + b + c

val x = sum _
x.apply(1,10,100)

val y = sum(1,_:Int,100)
y(4)

def echo(args: String*): Unit ={
    for(arg <- args) println(arg)
}

echo("hello")
echo("hello","world")

val arr = Array("What's","up","doc?")
echo(arr: _*)

def speed(distance: Float, time: Float): Float = distance/time

speed(100,10)
speed(time = 10 , distance = 100)