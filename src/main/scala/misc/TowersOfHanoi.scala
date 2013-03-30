package misc

object TowersOfHanoi {

  /*
  Towers Of Hanoi.

  There are three pegs: A, B and C. There are n disks. All disks are different in size.
  The disks are initially stacked on peg A so that they increase in size from the top to the bottom.
  The goal is to transfer the entire tower from the A peg to the C peg.
  One disk at a time can be moved from the top of a stack either to an empty peg or to
  a peg with a larger disk than itself on the top of its stack.

  The method returns a sequence of disk moves, each move is a String with two letters (A, B or C)
  corresponding to the peg the disk moves from and the peg it moves to.
  For example, the move "A-C" means that a top disk from peg A should be moved to peg C.
 */
  def transferFromAtoC(n: Int): List[String] = {

    def transferFrom(m: Int, from: Int, to: Int): List[String] = {
      if (m == 1) {        // single disk
        indexMoveToStringMove(from, to) :: Nil
      } else {
        transferFrom(m - 1, from, notFromOrTo(from, to)) ++: transferFrom(1, from, to) ++: transferFrom(m - 1, notFromOrTo(from, to), to)
      }
    }

    def notFromOrTo(i1: Int, i2: Int) = {
      if ((i1 == 0 && i2 == 1) || (i1 == 1 && i2 == 0)) 2
      else
      if ((i1 == 2 && i2 == 1) || (i1 == 1 && i2 == 2)) 0
      else
        1
    }

    def indexMoveToStringMove(from: Int, to: Int): String = {

      def indexToString(index: Int): String = {
        index match {
          case 0 => "A"
          case 1 => "B"
          case 2 => "C"
        }
      }

      indexToString(from) + "-" + indexToString(to)
    }

    transferFrom(n, 0, 2)
  }

}
