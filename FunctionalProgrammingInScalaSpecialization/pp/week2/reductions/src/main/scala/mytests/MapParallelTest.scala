package mytests

import common._
import org.scalameter.{Key, Warmer, config}
import scala.util.Random

object MapParallelTest {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val threshold = 10000

  def mapSeq[A](inp: Array[A], out: Array[A], from: Int, until: Int, f: A => A): Unit = {
    var idx = from
    while (idx < until) {
      out(idx) = f(inp(idx))
      idx = idx + 1
    }
  }

  def mapPar[A](inp: Array[A], out: Array[A], from: Int, until: Int, f: A => A): Unit = {
    if (until - from < threshold) {
      mapSeq(inp, out, from, until, f)
    } else {
      val mid = from + (until - from) / 2
      val t1 = task {
        mapPar(inp, out, from, mid, f)
      }
      mapPar(inp, out, mid, until, f)
      t1.join()
    }
  }

  def main(args: Array[String]): Unit = {
    val inp = Array.fill(10000000){Random.nextInt(10)}
    val out = Array.ofDim[Int](10000000)
    val parOut = Array.ofDim[Int](10000000)

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 40,
      Key.exec.benchRuns -> 80,
      Key.verbose -> true
    ) withWarmer(new Warmer.Default)

    val seqtime = standardConfig measure {
      mapSeq[Int](inp, out, 0, inp.length, x => x * x * x * x)
    }

    println(s"sequential count time: $seqtime ms")

    val partime = standardConfig measure {
      mapPar[Int](inp, parOut, 0, inp.length, x => x * x * x * x)
    }

    assert(out.deep == parOut.deep)

    println(s"parallel count time: $partime ms")

    println(s"speedup: ${seqtime / partime}")
  }
}
