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
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
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

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balance(i: Int, acc: Int): Boolean = {
      if (i == chars.length) {
        return acc == 0
      }
      if (acc < 0) {
        return false
      }
      balance(i + 1, acc + (if (chars(i) == '(') 1 else if (chars(i) == ')') -1 else 0))
    }
    balance(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      def balance(i: Int): Int = if (chars(i) == '(') 1 else if (chars(i) == ')') -1 else 0
      var sum = balance(idx)
      var mini = sum
      var i = idx + 1
      while (i < until) {
        sum += balance(i)
        mini = Math.min(mini, sum)
        i += 1
      }
      (sum, mini)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from < threshold) {
        return traverse(from, until, 0, 0)
      }
      val mid = from + (until - from) / 2
      val ((sum1, mini1), (sum2, mini2)) = parallel(reduce(from, mid), reduce(mid, until))
      (sum1 + sum2, Math.min(mini1, sum1 + mini2))
    }

    val (sum, mini) = reduce(0, chars.length)
    sum == 0 && mini >= 0
//    reduce(0, chars.length) == ???
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
