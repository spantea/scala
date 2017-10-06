import common._
import scala.util.Random
import java.util.Arrays

val xs = Array.fill(1000){Random.nextInt(500)}
val ys = new Array[Int](xs.length)
val maxDepth = 10

def sort(from: Int, until: Int, depth: Int): Unit = {
  if (depth == maxDepth) {
    Arrays.sort(xs, from, until - from)
  } else {
    val mid = (from + until) / 2
    parallel(sort(from, mid, depth + 1), sort(mid, until, depth + 1))

    val flip = (maxDepth - depth) % 2 == 0
    val src = if (flip) ys else xs
    val dst = if (flip) xs else ys
    merge(src, dst, from, mid, until)
  }
}

def merge(src: Array[Int], dst: Array[Int], from: Int, mid: Int, until: Int) {
  var left = from
  var right = mid
  var i = from
  while (left < mid && right < until) {
    while (left < mid && src(left) <= src(right)) {
      dst(i) = src(left)
      i += 1
      left += 1
    }
    while (right < until && src(right) <= src(left)) {
      dst(i) = src(right)
      i += 1
      right += 1
    }
  }
  while (left < mid) {
    dst(i) = src(left)
    i += 1
    left += 1

  }
  while (right < mid) {
    dst(i) = src(right)
    i += 1
    right += 1
  }
}

sort(0, xs.length, 0)

xs