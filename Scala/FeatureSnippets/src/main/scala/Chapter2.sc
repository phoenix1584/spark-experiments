1 + 2
val message : String = "test message"
// message = "test" // Should generate "reassignment to val" compile error.
var msg_mutable = "might change."
msg_mutable = "As promised, changing now"

def max(x : Int, y: Int) = if (x > y) x else y
max (-1,4)