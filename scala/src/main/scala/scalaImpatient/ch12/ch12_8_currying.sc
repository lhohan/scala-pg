def mulOneAtTheTime(x:Int) = (y:Int) => x * y

// Passing a value returns a function:
def multiplyBy6 = mulOneAtTheTime(6)
multiplyBy6(7)
//or:
mulOneAtTheTime(6)(7)

// currying: practical use: help type inference
val a = Array("Hello", "World")
val b = Array("Hello", "world")

a.corresponds(b)(_.equalsIgnoreCase(_))

