package sandbox.monads.errorhandling

import cats.MonadError
import sandbox.Logger

object MyMonadError extends App {

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    if(age >= 18) age.pure[F]
    else new IllegalArgumentException("Age must be greater than or equal to 18").raiseError[F, Int]

  import scala.util.Try
  import cats.instances.try_._
  validateAdult[Try](18).log()
  validateAdult[Try](8).log()

  import cats.instances.either._
  type ExceptionOr[A] = Either[Throwable, A]
  validateAdult[ExceptionOr](-1).log()
}
