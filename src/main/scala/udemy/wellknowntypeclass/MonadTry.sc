import cats._
import cats.implicits._
import scala.util._

//implicit val tryMonad: Monad[Try] = new Monad[Try] {
//  override def pure[A](x: A): Try[A] = Success(x)
//
//  override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] =
//    fa match {
//      case Success(a) => f(a)
//      case Failure(e) => Failure(e)
//    }
//
//  override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???
//}
//
//tryMonad.pure(5)
//tryMonad.pure(5).flatMap(i => tryMonad.pure(i + 1))
//tryMonad.pure(5).flatMap(i => Failure(new Exception("boom")))


//pure(x).flatMap(f) === f(x)
val f: Int => Try[Int] = i => Success(i)
Success(10).flatMap(f)
f(10)

val f: Int => Try[Int] = i => throw new Exception("boom")
Success(10).flatMap(f)
f(10)