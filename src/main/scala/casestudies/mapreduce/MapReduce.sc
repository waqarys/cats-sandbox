import cats.Monoid
import cats.syntax.semigroup._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

def foldMap[A, B: Monoid](as: Vector[A])(func : A => B): B =
  as.map(func).foldLeft(Monoid[B].empty)(_ |+| _)

def foldMap1[A, B : Monoid](as: Vector[A])(func: A => B): B =
  as.foldLeft(Monoid[B].empty)(_ |+| func(_))

import cats.{Monad, Monoid}
import cats.instances.int._
import cats.instances.future._

Monad[Future].pure(42)

val r = Monoid[Future[Int]].combine(Future(1), Future(2))
//  .onComplete({
//    case Success(value) => value
//    case Failure(exception) => exception
//  })

import scala.concurrent.duration._
Await.result(r, 1.second)
r

def parallelFoldMap[A, B : Monoid](values: Vector[A])(func: A => B): Future[B] = {
  // Calculate the number of items to pass to each CPU:
  val numCores = Runtime.getRuntime.availableProcessors
  val groupSize = (1.0 * values.size / numCores).ceil.toInt

  // Create one group for each CPU:
  val groups: Iterator[Vector[A]] =
    values.grouped(groupSize)

  // Create a future to foldMap each group:
  val futures: Iterator[Future[B]] =
    groups map { group =>
      Future {
        group.foldLeft(Monoid[B].empty)(_ |+| func(_))
      }
    }

  // foldMap over the groups to calculate a final result:
  Future.sequence(futures) map { iterable =>
    iterable.foldLeft(Monoid[B].empty)(_ |+| _)
  }

}

def parallelFoldMap1[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
  val numCores = Runtime.getRuntime.availableProcessors
  val groupSize = (1.0 * values.size / numCores).ceil.toInt
  val groups: Iterator[Vector[A]] =
    values.grouped(groupSize)
  val futures: Iterator[Future[B]] =
    groups.map(group => Future(foldMap(group)(func)))
  Future.sequence(futures) map { iterable =>
    iterable.foldLeft(Monoid[B].empty)(_ |+| _)
  }
}

val result: Future[Int] =
  parallelFoldMap((1 to 1000000).toVector)(identity)
Await.result(result, 1.second)
result

//cats
import cats.Monoid
import cats.instances.int._
// for Monoid
import cats.instances.future._ // for Applicative and Monad
import cats.instances.vector._ // for Foldable and Traverse
import cats.syntax.foldable._// for combineAll and foldMap
import cats.syntax.traverse._// for traverse
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

def parallelFoldMapCats[A, B: Monoid]
(values: Vector[A])
(func: A => B): Future[B] = {
  val numCores
  = Runtime.getRuntime.availableProcessors
  val groupSize = (1.0 * values.size / numCores).ceil.toInt
  values
    .grouped(groupSize)
    .toVector
    .traverse(group => Future(group.toVector.foldMap(func)))
    .map(_.combineAll)
}
val future: Future[Int] =
  parallelFoldMapCats((1 to 1000).toVector)(_ * 1000)
Await.result(future, 1.second)
future