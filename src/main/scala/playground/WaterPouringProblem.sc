class WaterPouringProblem(capacity: Vector[Int]) {

  // States
  type State = Vector[Int]
  val initialState: Vector[Int] = capacity map (x => 0)

  // Moves
  trait Move {
    // A method that defines state changes
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    def change(state: State): State = state updated (glass, 0) // Remember: updated creates a new vector with an element changed
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State): State = state updated (glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State): State = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated (to, state(to) + amount) updated (from, state(from) - amount)
    }
  }

  val glasses: Range = capacity.indices

  val moves: Seq[Move] =
  // for g 'taken from' glasses
    (for (g <- glasses) yield Empty(g)) ++ // Empty a glass
      (for (g <- glasses) yield Fill(g)) ++ // Fill up a glass
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  // Paths, sequences of moves
  class Path(history: List[Move], val endState: State) { // val: Available from outside
    def extend(move: Move) = new Path(move :: history, move change endState)
    override def toString: String = (history.reverse mkString " ") + "--> " + endState
  }

  val initialPath = new Path(Nil, initialState)

  // Path generating function (state exploration function)
  def from(paths: Set[Path], explored: Set[State]): LazyList[Set[Path]] =
    if (paths.isEmpty) LazyList.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState) // Restrict moves to states that are unexplored
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets: LazyList[Set[Path]] = from(Set(initialPath), Set(initialState))

  def solutions(target: Int): LazyList[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}

val problem = new WaterPouringProblem(Vector(4, 9))
val solution = problem.solutions(6)
println(solution.take(1).toList)