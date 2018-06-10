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
  ) withWarmer (new Warmer.Default)

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
    if (chars.isEmpty) true
    var count = 0;

    for (i <- chars.indices) {
      val c = chars(i)
      if (c == '(') count += 1
      else if (c == ')') count -= 1
      if (count < 0) return false
    }
    if (count == 0) true else false

  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, total: Int, least: Int): (Int, Int) = {
      var i = idx
      var t = total
      var l = least
      while (i < until) {
        val c = chars(i)
        if ('(' == c) {
          t += 1
        }else if (')' == c) {
          t -= 1
        }
        l = if(l == 0) t else Math.min(l, t)
        i+=1
      }
      (t, l)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = (until - from) / 2
        val (l, r) = parallel(reduce(from, from + mid), reduce(from + mid, until))
        val cumTot = l._1 + r._1
        val cumLeast = l._1 + Math.min(l._2, r._2)
        (cumTot, cumLeast)
      }
    }

    reduce(0, chars.length) == (0,0)
  }


  // For those who want more:
  // Prove that your reduction operator is associative!

}
