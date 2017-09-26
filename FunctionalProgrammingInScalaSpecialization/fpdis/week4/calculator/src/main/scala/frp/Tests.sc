import calculator.Signal

def computeDelta(a: Signal[Double], b: Signal[Double],
                 c: Signal[Double]): Signal[Double] = {
  Signal(Math.pow(b(), 2) - 4 * a() * c())
}

def computeSolutions(a: Signal[Double], b: Signal[Double],
                     c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

  val x1: Double = (-b() + Math.sqrt(Math.abs(computeDelta(a, b, c)()))) / 2 * a()
  val x2: Double = (-b() - Math.sqrt(Math.abs(computeDelta(a, b, c)()))) / 2 * a()
  Signal(Set(x1, x2))
}

computeDelta(Signal(2), Signal(2), Signal(2))()

computeSolutions(Signal(2), Signal(2), Signal(2), Signal(-12))()

val mp = Map() updated (1, "A") updated (2, "B") updated (3, "C")

for ((k, v) <- mp; y = v + v) yield (k, y)