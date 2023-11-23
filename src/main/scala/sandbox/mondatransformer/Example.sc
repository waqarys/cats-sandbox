import cats.data.{EitherT, OptionT}

type ListOption[A] = OptionT[List, A]

import cats.instances.list._     //for Monad
import cats.syntax.applicative._ //for pure

val result1: ListOption[Int] = OptionT(List(Option(10)))
val result2: ListOption[Int] = 32.pure[ListOption]

val r = result1.flatMap { (x: Int) =>
  result2.map { (y: Int) =>
    x + y
  }
}

r.value

type ErrorOr[A] = Either[String, A]
type ErrorOrOption[A] = OptionT[ErrorOr, A]

import cats.instances.either._

val a: ErrorOrOption[Int] = 10.pure[ErrorOrOption]
val b = 32.pure[ErrorOrOption]

val c = a.flatMap(x => b.map(y => x + y))
c.value

import cats.instances.option._  //for Monad
123.pure[EitherT[Option, String, _]]