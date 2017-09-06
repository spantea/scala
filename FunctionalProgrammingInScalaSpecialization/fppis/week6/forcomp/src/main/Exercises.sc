import nqueens.queens


def removeAt[T](n: Int, xs: List[T]): List[T] = n match {
  case x if x <= 0 => xs.tail
  case x if x > 0 => xs.head :: removeAt(n - 1, xs.tail)
}

removeAt(1, List(1, 2, 3))


def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => List()
  case x :: xs1 => x match {
    case y :: ys => flatten(y :: ys ::: xs1) // unfold first elem if it is a list
    case y => y :: flatten(xs1)
  }
}

flatten(List(1, List(2, 4), 3))


val list = List(List(1, 1), 2, List(3, List(5, 8)))

flatten(list)

def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
  (for {
    (x, y) <- xs zip ys
  } yield x * y) sum
}

List(1, 2, 3).sum



val sol = List(0, 2, 1)

for {
  c <- sol
  r <- 3 until 0 by -1
} yield (r, c)

//sol zip (sol.length until 0 by -1)

object nqueens {

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] =
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    placeQueens(n)
  }

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRows = (row -1 to 0 by -1) zip queens
    queensWithRows.forall {
      case (r, c) => c != col && math.abs(col - c) != row - r
    }
  }

  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
        yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }
}

nqueens.show(queens(4).head)

List(1, 2, 3, 4) mkString " pula "

List("Every", "student", "likes", "Scala").groupBy((element: String) => element.length)




