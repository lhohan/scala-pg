package misc

import org.scalatest.FunSuite

class TowersOfHanoiTest extends FunSuite {

  import misc.TowersOfHanoi.transferFromAtoC

  test("test moves needed to move single tower") {
    def towerMoves = transferFromAtoC(1)
    assert("A-C" === towerMoves(0))
  }

  test("test moves needed to move 2 towers") {
    def towerMoves = transferFromAtoC(2)
    assert("A-B" === towerMoves(0))
    assert("A-C" === towerMoves(1))
    assert("B-C" === towerMoves(2))
  }

  test("test moves needed to move 3 towers") {
    def towerMoves = transferFromAtoC(3)
    assert(towerMoves.length == 7)
    assert("A-C" === towerMoves(0))
    assert("A-B" === towerMoves(1))
    assert("C-B" === towerMoves(2))
    assert("A-C" === towerMoves(3))
    assert("B-A" === towerMoves(4))
    assert("B-C" === towerMoves(5))
    assert("A-C" === towerMoves(6))
  }

  test("test count moves needed to move 5 towers") {
    def towerMoves = transferFromAtoC(5)
    assert("A-C" === towerMoves(0))
    assert("A-B" === towerMoves(1))
    assert("C-B" === towerMoves(2))
    assert("A-C" === towerMoves(3))
    assert("B-A" === towerMoves(4))
    assert("B-C" === towerMoves(5))
    assert("A-C" === towerMoves(6))
    assert("A-B" === towerMoves(7))
    assert("C-B" === towerMoves(8))
    assert("C-A" === towerMoves(9))
    assert("B-A" === towerMoves(10))
    assert("C-B" === towerMoves(11))
    assert("A-C" === towerMoves(12))
    assert("A-B" === towerMoves(13))
    assert("C-B" === towerMoves(14))
    assert("A-C" === towerMoves(15))
    assert("B-A" === towerMoves(16))
    assert("B-C" === towerMoves(17))
    assert("A-C" === towerMoves(18))
    assert("B-A" === towerMoves(19))
    assert("C-B" === towerMoves(20))
    assert("C-A" === towerMoves(21))
    assert("B-A" === towerMoves(22))
    assert("B-C" === towerMoves(23))
    assert("A-C" === towerMoves(24))
    assert("A-B" === towerMoves(25))
    assert("C-B" === towerMoves(26))
    assert("A-C" === towerMoves(27))
    assert("B-A" === towerMoves(28))
    assert("B-C" === towerMoves(29))
    assert("A-C" === towerMoves(30))
    assert(towerMoves.length == 31)
  }

  test("test count moves needed to move 8 towers") {
    def towerMoves = transferFromAtoC(8)
    assert(towerMoves.length == 255)
  }

}
