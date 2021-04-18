package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 1000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing
    extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    def iter(chars: Array[Char], tally: Int): Boolean =
      if (tally < 0) false
      else if (chars.isEmpty) tally == 0
      else {
        val change = chars.head match {
          case '(' => 1
          case ')' => -1
          case _   => 0
        }
        iter(chars.tail, tally + change)
      }
    iter(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) =
      if (idx >= until) (arg1, arg2)
      else {
        val newArg1 = arg1 + chars(idx) match {
          case '(' => 1
          case ')' => -1
          case _   => 0
        }
        traverse(idx + 1, until, newArg1, Math.min(newArg1, arg2))
      }

    def reduce(from: Int, until: Int): (Int, Int) =
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val m = (until + from) / 2
        val ((tally1, minTally1), (tally2, minTally2)) = parallel(
          reduce(from, m),
          reduce(m, until)
        )
        (tally1 + tally2, Math.min(minTally1, minTally2 + tally1))
      }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
