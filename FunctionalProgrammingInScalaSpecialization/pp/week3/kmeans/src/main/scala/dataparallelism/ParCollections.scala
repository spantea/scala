package dataparallelism

import java.util.concurrent.ConcurrentSkipListSet

import scala.collection.{GenSeq, GenSet}

class ParCollections {
  def largestPalindrome(xs: GenSeq[Int]): Int = {
    xs.aggregate(Integer.MIN_VALUE)(
      (largest, n) => if (isPalindrome(n) && n > largest) n else largest, (x, y) => if (x > y) x else y
    )
  }

  def isPalindrome(i: Int): Boolean = {
    ???
  }

  def intersection(a: GenSet[Int], b: GenSet[Int]) = {
    val result = new ConcurrentSkipListSet[Int]()
    for (x <- a) if (b contains x) result.add(x)
    result
  }

  def intersectionElegant(a: GenSet[Int], b: GenSet[Int]) = {
    if (a.size < b.size) a.filter(b(_))
    else b.filter(a(_))
  }

  def main(args: Array[String]): Unit = {
    val array = (1 to 10000000).toArray
    largestPalindrome(array)
    largestPalindrome(array.par)
  }
}
