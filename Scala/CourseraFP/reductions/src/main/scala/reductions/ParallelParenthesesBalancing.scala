package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceAcc(chars: Array[Char], acc: Int): Boolean = {
      if (chars.isEmpty) {
        acc == 0
      }
      else {
        val h = chars.head
        val n = if (h == '(') acc + 1 else if (h == ')') acc - 1 else acc

        if (n >= 0) balanceAcc(chars.tail, n)
        else false
      }
    }
    balanceAcc(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    /**
      *
      * @param idx : zero-based begin index
      * @param until : zero-based end index
      * @param arg1 : # of unmatched '(' parenthesis
      * @param arg2 : # of unmatched ')' parenthesis
      * @return (# of unmatched '(' parenthesis, # of unmatched ')' parenthesis)
      */
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      if (idx < until) {
        chars(idx) match {
          case '(' => traverse(idx + 1, until, arg1 + 1, arg2)
          case ')' =>
            if (arg1 > 0) traverse(idx + 1, until, arg1 - 1, arg2) // decreasing the nb of unmatched '(' parenthesis
            else traverse(idx + 1, until, arg1, arg2 + 1) // increasing the nb of unmatched ')' parenthesis since there are no unmatched '(' parenthesis
          case _ => traverse(idx + 1, until, arg1, arg2)
        }
      } else {
        (arg1, arg2)
      }
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      }
      else {
        val middle = from + (until - from) / 2
        val ((a1, a2), (b1, b2)) = parallel[(Int, Int), (Int, Int)](reduce(from, middle), reduce(middle, until))

        // no matter the a1 - b2 result, there is atleast : (b1 + xxx, a2 + yyy)
        // because 'b1' unmatched '(' parenthesis from the right task can't be compensated since it's the RIGHT task ; same for 'a2' unmatched ')' parenthesis
        // depending on a1 - b2 comparaison, xxx will be null and yyy will be valued b2 - a1 and vice-versa

        if (a1 > b2) {
          // )))((())(( => )))((( ==> (3, 3)
          (a1 - b2 + b1, a2)
          // left tuple : the 'a1' unmatched '(' parenthesis are matched with the 'b2' unmatched ')' parenthesis, to which we add the 'b1' unmatched '(' parenthesis from the right computation
          // right tuple : hence the only remaining unmatched ')' parenthesis is 'a2' ('b2' having been already used by subtracting it to 'a1')
        } else {
          // )))(()))(( => ))))(( ==> (2, 4)
          (b1, b2 - a1 + a2)
        }
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
