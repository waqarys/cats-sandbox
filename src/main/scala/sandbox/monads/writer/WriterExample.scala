package sandbox.monads.writer

import cats.Id
import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import sandbox.WriterLog  //Pure

object WriterExample extends App {

  type Logged[A] = Writer[Vector[String], A]
  123.pure[Logged]

  import cats.syntax.writer._

  val a = Writer(
    Vector("msg1", "msg2", "msg3"),
    123
  )
  val b = 123.writer(Vector("msg1", "msg2", "msg3"))

  val aResult: Id[Int] = a.value
  val aLog: Id[Vector[String]] = a.written

  val (log, result) = a.run

  //Composing and Transforming Writers

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  println(writer1.run)

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
  writer2.logW()

  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100
  )
  println(writer3.run)
  writer3.logW()

}
