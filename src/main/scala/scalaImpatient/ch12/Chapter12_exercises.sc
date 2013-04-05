
/**
 * Misc code to support the exercises.
 **/

//ex. 6 - def adjustToPair(function: (Int, Int) => Int) : (Tuple2[Int,Int] => Int) = {}
def adjustToPair(function: (Int, Int) => Int)(pair: Tuple2[Int, Int]) = function(pair._1, pair._2)

adjustToPair(_ * _)((6, 7))

// ex. 9
def corresponds[A, B](this_ : Seq[A], that: Seq[B])(p: (A, B) => Boolean): Boolean = {
  val i = this_.iterator
  val j = that.iterator
  while (i.hasNext && j.hasNext)
    if (!p(i.next(), j.next()))
      return false

  !i.hasNext && !j.hasNext
}


