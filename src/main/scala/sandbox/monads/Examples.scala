package sandbox.monads

import sandbox.Logger

object Examples extends App {

  import cats.Monad
  import cats.syntax.functor._  //for map
  import cats.syntax.flatMap._  //for flatMap

//  def sumSquares[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
//    a.flatMap(x => b.map(y => x*x + y*y))

  def sumSquares[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for{
      x <- a
      y <- b
    } yield x*x + y*y

  import cats.instances.option._
  sumSquares(Option(3), Option(4)).log()

  import cats.instances.list._
  sumSquares(List(1,2,3), List(4,5)).log()
}
