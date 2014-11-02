package misc

import scala.reflect.runtime.universe._


/**
 * Created by hans on 28/09/14.
 */
object TypeTags {

    class Foo
    class Bar extends Foo

    def doSomethingWithLists[A : TypeTag](list : List[A]) = typeOf[A] match {
      case t if t =:= typeOf[String] => "type of strings"
      case t if t <:< typeOf[Foo] => "type of foo"
    }


    def doSomethingWithMaps[A: TypeTag, B:TypeTag](m: Map[A,B]) = {
      (typeOf[A],typeOf[B]) match {
        case (tk, tv) if tk =:= typeOf[String] && tv =:= typeOf[Int] => "key:String,value:Int"
        case (tk, tv) if tk =:= typeOf[String] && tv <:< typeOf[AnyVal] => "key:String,value:AnyVal"
        case (tk, tv) if tk =:= typeOf[String]  => "key:String"
        case _ => ???
      }
    }


}
