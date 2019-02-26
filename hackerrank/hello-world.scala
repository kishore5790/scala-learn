// repeat string n times
object Solution extends App {
 def f(n: Int): List[Nothing] = {
    @annotation.tailrec
    def loop(current: Int, max: Int): List[Nothing] = {
        if(current <= max) println("Hello World")
        else return(Nil)
        loop(current + 1, max)
    }
    loop(1, n)
 }

  var n = scala.io.StdIn.readInt
  f(n)
}

