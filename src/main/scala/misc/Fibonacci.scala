package misc

object Fibonacci {

  def generateFibonacci(n: Int): List[Int] = {

    def fibIter(f1: Int, f2: Int, m: Int): List[Int] = {
      if (m ==0)
        Nil
      else
        f1 :: fibIter(f2, f1+f2,m-1)
    }
    fibIter(1,1,n)
  }

}
