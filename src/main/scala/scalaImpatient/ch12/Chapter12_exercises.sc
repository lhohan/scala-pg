

//def adjustToPair(function: (Int, Int) => Int) : (Tuple2[Int,Int] => Int) = {}
def adjustToPair(function: (Int, Int) => Int)(pair: Tuple2[Int, Int]) = function(pair._1, pair._2)

adjustToPair(_ * _)((6, 7))