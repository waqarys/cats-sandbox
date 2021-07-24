package sandbox.monads.writer

object ParallelFactorial extends App {

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  factorial(5)

  //Parallel Execution
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._

  println("Parallel Execution")
  Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(5))
  )), 5.seconds)
}
