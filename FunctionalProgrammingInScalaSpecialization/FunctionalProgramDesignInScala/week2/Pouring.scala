class Pouring(capacity: Vector[Int]) {

  // States
  type State = Vector[Int]

  val initialState = capacity map (x => 0)

  //Moves

  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    def change(state: State) = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State) = state updated (glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State) = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amount) updated (to, state(from) + amount)
    }
  }

  val glasses = 0 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  // Paths

  class Path(history: List[Move]) {
    def endState: State = trackState(history)
    //def endState: State = (history foldRight initialState)(_ change _)

    def extend(move: Move) = new Path(move :: history)

    private def trackState(xs: List[Move]): State = xs match {
      case Nil => initialState
      case move :: xs1 => move change trackState(xs1)
    }

    override def toString = (history.reverse mkString " ") + "---> " + endState
  }

  val initailPath = new Path(Nil)

  def from(paths: Set[Path]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
      } yield next
      paths #:: from(more)
    }
}
