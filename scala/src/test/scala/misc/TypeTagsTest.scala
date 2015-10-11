package misc

import org.scalatest.FunSuite

/**
 * Created by hans on 28/09/14.
 */
class TypeTagsTest extends FunSuite{
  import TypeTags._

  test("example stack overflow using list"){
    assertResult("type of strings", "list of strings should be of type list of strings")(doSomethingWithLists(List("i_am_a_string")))
    assertResult("type of foo", "list of bars should be of type list of foos")(doSomethingWithLists(List(new Bar)))
  }

  test("type tags - maps"){
    assertResult("key:String" ,"type of key is string")(doSomethingWithMaps(Map("i_am_a_string" -> "another_string")))
    assertResult("key:String,value:Int" ,"type of key is string, type of value is int")(doSomethingWithMaps(Map("i_am_a_string" -> 5)))
    assertResult("key:String,value:AnyVal" ,"type of key is string, type of value is double"){
      doSomethingWithMaps(Map("i_am_a_string" -> 5.4))
    }
  }

}
