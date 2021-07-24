package sandbox.monads.writer

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object WriterParallelFactorial extends App {

  import cats.data.Writer
  import cats.instances.vector._
  import cats.syntax.applicative._  //for pure

  type Logged[A] = Writer[Vector[String], A]

  42.pure[Logged] // WriterT(Vector(), 42)

  import cats.syntax.writer._   //for tell
  Vector("Message").tell  //WriterT(Vector("Message"), ())

  import cats.instances.vector._  //for Monoid
  41.pure[Logged].map(_ + 1)

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] =
    for {
      ans <- if(n == 0){
            1.pure[Logged]
          } else {
            slowly(factorial(n - 1).map(_ * n))
          }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  val (log, res) = factorial(5).run
  println(s"ANS: $res, LOG: $log")

  println("Parallel Execution")
  Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(5))
  )).map(_.map(_.written.map(println))), 5.seconds)
}
