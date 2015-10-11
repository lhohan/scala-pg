import org.scalatest.FunSuite

class AssociationsTest extends FunSuite {
  test("no domain") {
    assertResult("")(Associations.findFrom("S", ""))
  }
  
  test("from-node not in domain") {
    assertResult("")(Associations.findFrom("S", "A -> B"))
  }
  
  test("from-node in domain: no associations") {
    assertResult("")(Associations.findFrom("S", "A -> B;S"))
  }
  
  test("from-node in domain: 1 valid association") {
    assertResult("A->S")(Associations.findFrom("S", "A -> B;A -> S"))
  }
}