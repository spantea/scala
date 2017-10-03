package scalashop

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
    *  starting with `from` and ending with `end` (non-inclusive).
    *
    *  Within each row, `blur` traverses the pixels by going from left to right.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var i = 0
    var j = 0

    while (j < src.height) {
      while (i < src.width) {
        if (j >= from && j < end) {
          dst.update(i, j, boxBlurKernel(src, i, j, radius))
        }
        i += 1
      }
      i = 0
      j += 1
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
    *
    *  Parallelization is done by stripping the source image `src` into
    *  `numTasks` separate strips, where each strip is composed of some number of
    *  rows.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    var t = 0
    val stripSize = src.height / numTasks
    var row = 0; var strip = 0

    while (t < numTasks) {
      val computation = task {
        while (strip < stripSize || (t == (numTasks - 1) && row < src.height)) {
          var i = 0
          while (i < src.width) {
            dst.update(i, row, boxBlurKernel(src, i, row, radius))
            i += 1
          }
          strip += 1
          row += 1
        }
        strip = 0
      }
      computation.join()
      t += 1
    }
  }
}
