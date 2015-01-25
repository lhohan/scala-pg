package misc

/**
 * This a Scala implementation of the Game of Life based on the Haskell implementation given in Graham Hutton's excellent book: Programming in Haskell.
 * */

class GameOfLife {
  val width = 5
  val height = 5

  type Pos = (Int, Int)
  type Board = List[Pos]

  val Glider = List((4,2), (2,3), (4,3), (3,4), (4,4))

  def seqn = ()

  def showcells(b: Board): Unit = ()
}
