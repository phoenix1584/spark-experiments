// Mutable objects
class Time{
  private[this] var h = 12
  private[this] var m = 0

  def hour: Int = h
  def hour_= (x: Int) = {
    require(x >= 0 && x < 24)
    h = x
  }

  def minute =m
  def minute_=(x:Int) = {
    require(x >= 0 && x < 60)
    m =x
  }
}

// TODO : Digital circuit simulation DSL Example Section 18.4 onwards