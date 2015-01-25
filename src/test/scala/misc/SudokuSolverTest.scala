package misc

import org.scalatest.FunSuite

import scala.concurrent.{TimeoutException, Await, Future}

/**
 * Created by hans on 25/01/15.
 */
class SudokuSolverTest extends FunSuite {

  import misc.SudokuSolver._

  test("some stuff") {
    val m: Matrix[Int] = Stream(Stream(1, 2, 3), Stream(4, 5, 6))
    val g: Grid = Stream(Stream(1, 2, 3))
  }

  test("cartesian product") {
    val arg = Stream(Stream(1, 2), Stream(3), Stream(4, 5))
    val result = cartesianProduct(arg)
    assertResult(Stream(Stream(1, 3, 4), Stream(1, 3, 5), Stream(2, 3, 4), Stream(2, 3, 5))) {
      //      println(s"    ${cartesianProduct(result).map(_.toList).toList}")
      result
    }
  }

  test("rows") {
    assertResult(Stream(1, 7, 2, 5, 4, 9, 6, 8, 3)) {
      rows(solved).head
    }
  }

  test("cols") {
    assertResult(Stream(1, 6, 3, 4, 8, 2, 9, 7, 5)) {
      cols(solved).head
    }
  }

  test("boxes") {
    assertResult(Stream(1, 7, 2, 6, 4, 5, 3, 8, 9)) {
      boxs(solved).head
    }
  }

  test("ungroup") {
    assertResult(Stream(1, 7, 2, 6, 4, 5, 3, 8, 9)) {
      ungroup(Stream(Stream(1, 7, 2), Stream(6, 4, 5), Stream(3, 8, 9)))
    }
  }

  test("group") {
    assertResult(Stream(Stream(1, 7, 2), Stream(6, 4, 5), Stream(3, 8, 9))) {
      group(Stream(1, 7, 2, 6, 4, 5, 3, 8, 9))
    }
  }

  test("nodups") {
    assertResult(true) {
      noDups(Stream(1, 4, 5, 6, 7))
    }
    assertResult(true) {
      noDups(Stream())
    }
    assertResult(true) {
      noDups(Stream(1))
    }
    assertResult(false) {
      noDups(Stream(2, 3, 5, 2))
    }
  }

  test("solve - version one of solver, trivial case") {
    assertResult(solved) {
      solve(Stream(
        Stream(1, 7, 2, 5, 4, 9, 6, 8, 3),
        Stream(6, 4, 5, 8, 7, 3, 2, 1, 9),
        Stream(3, 8, 9, 2, 6, 1, 7, 4, 5),
        Stream(4, 9, 6, 3, 2, 7, 8, 5, 1),
        Stream(8, 1, 3, 4, 5, 6, 9, 7, 2),
        Stream(2, 5, 7, 1, 9, 8, 4, 3, 6),
        Stream(9, 6, 4, 7, 1, 5, 3, 2, 8),
        Stream(7, 3, 1, 6, 8, 2, 5, 9, 4),
        Stream(5, 2, 8, 9, 3, 4, 1, 6, 7)
      )).toList.head
    }
  }


  val X = Blank

  import scala.concurrent.duration._

  def printTime(value: Long, s: String) = println(s"== Time $value ms for $s")

  test("solve - version one of solver, one blanked") {
    assertResult(solved) {
      val (elapsed, result) = time(solve(Stream(
        Stream(X, 7, 2, 5, 4, 9, 6, 8, 3),
        Stream(6, 4, 5, 8, 7, 3, 2, 1, 9),
        Stream(3, 8, 9, 2, 6, 1, 7, 4, 5),
        Stream(4, 9, 6, 3, 2, 7, 8, 5, 1),
        Stream(8, 1, 3, 4, 5, 6, 9, 7, 2),
        Stream(2, 5, 7, 1, 9, 8, 4, 3, 6),
        Stream(9, 6, 4, 7, 1, 5, 3, 2, 8),
        Stream(7, 3, 1, 6, 8, 2, 5, 9, 4),
        Stream(5, 2, 8, 9, 3, 4, 1, 6, 7)
      )).toList, 2 seconds)
      printTime(elapsed, "version one of solver, one blanked")
      result.head
    }
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  def time[A](x: => A, timeout: Duration = 20 seconds): (Long, A) = {
    val start = System.currentTimeMillis()
    val result = Await.result(Future(x), timeout)
    val end = System.currentTimeMillis()
    (end - start, result)
  }

  test("pruneRow") {
    val choices: Row[Choices] = Stream(
      Stream(3),
      Stream(1, 2, 3, 4, 5),
      Stream(2),
      Stream(5)
    )
    assertResult(Stream(
      Stream(3),
      Stream(1, 4),
      Stream(2),
      Stream(5))
    ) {
      pruneRow(choices)
    }

  }

    test("solve - pruning solver, with significant improvement over initial solver (more than 5 or 6 blanks runs a lot longer than 30 seconds)") {
      assertResult(solved){
        val (elapsed, result) = time(solve2(sudokuSinglePrune).toList, 30 seconds)
        printTime(elapsed,"pruning solver")
        result.head
      }
    }

  test("solve - repeater pruning solver") {
      assertResult(solved){
        val (elapsed, result) = time(solve3(sudokuSinglePrune).toList, 30 seconds)
        printTime(elapsed,"repeated pruning solver")
        result.head
      }
    }

  def mkString: Grid => String = g => "\n" + g.map(_.mkString("|")).mkString("\n")

  lazy val solved: Grid = Stream(
    Stream(1, 7, 2, 5, 4, 9, 6, 8, 3),
    Stream(6, 4, 5, 8, 7, 3, 2, 1, 9),
    Stream(3, 8, 9, 2, 6, 1, 7, 4, 5),
    Stream(4, 9, 6, 3, 2, 7, 8, 5, 1),
    Stream(8, 1, 3, 4, 5, 6, 9, 7, 2),
    Stream(2, 5, 7, 1, 9, 8, 4, 3, 6),
    Stream(9, 6, 4, 7, 1, 5, 3, 2, 8),
    Stream(7, 3, 1, 6, 8, 2, 5, 9, 4),
    Stream(5, 2, 8, 9, 3, 4, 1, 6, 7)
  )

  lazy val sudokuSinglePrune: Grid = Stream(
    Stream(X, X, X, X, X, X, 6, 8, X),
    Stream(X, X, X, X, 7, 3, 2, 1, 9),
    Stream(3, X, 9, 2, 6, X, 7, 4, 5),
    Stream(4, 9, X, 3, 2, X, 8, 5, 1),
    Stream(8, 1, 3, 4, 5, X, 9, 7, 2),
    Stream(X, X, X, X, X, X, X, 3, 6),
    Stream(9, 6, X, 7, 1, X, 3, 2, 8),
    Stream(7, 3, X, 6, 8, X, 5, 9, 4),
    Stream(5, 2, 8, 9, 3, X, 1, 6, 7)
  )

  lazy val sudoku = Stream(
    Stream(X, X, X, X, X, X, 6, 8, X),
    Stream(X, X, X, X, 7, 3, X, X, 9),
    Stream(3, X, 9, X, X, X, X, 4, 5),
    Stream(4, 9, X, X, X, X, X, X, X),
    Stream(8, X, 3, X, 5, X, 9, X, 2),
    Stream(X, X, X, X, X, X, X, 3, 6),
    Stream(9, 6, X, X, X, X, 3, X, 8),
    Stream(7, X, X, 6, 8, X, X, X, X),
    Stream(X, 2, 8, X, X, X, X, X, 7)
  )

}
