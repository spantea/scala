package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(Math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    val x1: Double = (-b() + Math.sqrt(Math.abs(delta()))) / (2 * a())
    val x2: Double = (-b() - Math.sqrt(Math.abs(delta()))) / (2 * a())
    Signal(Set(x1, x2))
  }
}
