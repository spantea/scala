package pouring

object PouringTest extends App {
  val problem = new Pouring(Vector(4, 7))
  println(problem.moves)

  problem.solutions(6).take(6)
}
