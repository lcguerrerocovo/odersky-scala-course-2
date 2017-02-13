package week2

/**
  * Created by luisguerrero on 1/11/17.
  */
class Pouring(capacity: Vector[Int]) {

  type State = Vector[Int]
  val initialState = capacity map (x => 0)

  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    def change(state: State) = state updated(glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    def change(state: State) = state updated(glass, capacity(glass))
  }

  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State) = {
      val toMove = state(from) min (capacity(to) - state(to))
      state updated(to, state(to) + toMove) updated(from, state(from) - toMove)
    }
  }

  val glasses = 0 until capacity.length

  val moves =
    (for (glass <- glasses) yield Empty(glass)) ++
      (for (glass <- glasses) yield Fill(glass)) ++
      (for {x <- glasses
            y <- glasses
            if (x != y)} yield Pour(x, y))

  class Path(history: List[Move]) {

    def endState = (history foldRight initialState) (_ change _)

    def extend(move: Move) : Path = new Path(move :: history)

    override def toString = (history.reverse mkString " -> ") + " -> " + endState
    //def endState = trackState(history)

    //def trackState(xs: List[Move]) : State = xs match {
    //  case Nil => initialState
    //  case move :: xs1 => move change trackState(xs1)
    //}
  }

}

object Test extends App {
    // getting 6 liters of water
    val pouring = new Pouring(Vector(9,4))
    val path = new pouring.Path(List(pouring.Fill(0),
      pouring.Pour(0,1), pouring.Empty(1),
      pouring.Pour(0,1), pouring.Empty(1),
      pouring.Pour(0,1), pouring.Fill(0),
      pouring.Pour(0,1), pouring.Empty(1)).reverse)

      println(path.toString)
}
