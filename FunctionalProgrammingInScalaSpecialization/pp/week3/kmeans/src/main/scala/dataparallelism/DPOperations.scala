package dataparallelism

class DPOperations {
  def sum(xs: Array[Int]): Int = {
    xs.par.foldLeft(0)(_ + _) // not parallel
    xs.par.fold(0)(_ + _) //parallel
  }

  def max(xs: Array[Int]): Int = {
    xs.par.fold(Integer.MIN_VALUE)((x: Int, y: Int) => if (x > y) x else y)
  }

  def isVowel(c: Char): Boolean = {
    true
  }

  def main(args: Array[String]): Unit = {
    Array('A', 'P', 'Z', 'I', 'U').aggregate(0)((count, c) => if (isVowel(c)) count + 1 else count, _ + _)
  }
}
