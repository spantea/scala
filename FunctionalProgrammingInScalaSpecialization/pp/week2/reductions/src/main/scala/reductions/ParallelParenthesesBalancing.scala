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
    val length = 10000000
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
    def loop(acc: Int, index: Int): Boolean = {
      if (index >= chars.length) acc == 0
      else if (acc < 0) false
      else if (chars(index) == '(') loop(acc + 1, index + 1)
      else if (chars(index) == ')') loop(acc - 1, index + 1)
      else loop(acc, index + 1)
    }
    loop(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, left: Int, right: Int): (Int, Int) = {
        if (idx >= until) (left, right)
        else if (chars(idx) == '(') traverse(idx + 1, until, left + 1, right)
        else if (chars(idx) == ')' && left > 0) traverse(idx + 1, until, left - 1, right)
        else if (chars(idx) == ')') traverse(idx + 1, until, left, right + 1)
        else traverse(idx + 1, until, left, right)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        val (l, r) = traverse(from, until, 0, 0)
        (l, r)
      } else {
        val mid = from + (until - from) / 2

        val (t1Res, t2Res) = parallel(reduce(from, mid), reduce(mid, until))

        val l = if (t2Res._2 > t1Res._1) t2Res._1 else t1Res._1 - t2Res._2 + t2Res._1
        val r = if (t2Res._2 < t1Res._1) t1Res._2 else t2Res._2 - t1Res._1 + t1Res._2
        (l, r)
      }
    }
    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
