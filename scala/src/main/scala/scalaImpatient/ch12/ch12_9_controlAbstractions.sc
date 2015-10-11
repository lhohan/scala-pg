def runInThread(block: () => Unit) {
  new Thread {
    override def run {
      block()
    }
  }.start()
}

runInThread(() => println("hello"))

// same using call by name notation

def runInThread2(block :  => Unit) {
  new Thread {
    override def run {
      block
    }
  }.start()
}

runInThread2(println("hello"))

def until(condition : => Boolean)(block : => Unit){
  if(!condition){
    block
    until(condition)(block)
  }
}

var x:Int = 0
until(x==5){
  x += 1; // need this ';' to indicate new stmt (?)
  println(x)
}






