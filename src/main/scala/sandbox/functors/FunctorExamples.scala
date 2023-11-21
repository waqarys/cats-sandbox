package sandbox.functors

object FunctorExamples extends App{

  implicit class Logger[A](value: A){
    def log()= println(value.toString)
  }

  import cats.Functor
  import cats.instances.list._
  import cats.instances.option._

  val list1 = List(1, 2, 3)
  val list2 = Functor[List].map(list1)(_ * 2)

  val option1 = Option(123)
  val option2= Functor[Option].map(option1)(_.toString)

  //Functor lift
  //A => B  -----> F[A] => F[B]
  val func = (x: Int) => x + 1
  val liftedFunc = Functor[Option].lift(func)
  liftedFunc.apply(Option(2)).log
  liftedFunc(Option(1)).log

  import cats.syntax.functor._
  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]) = {
    start.map(n => n + 1 * 2)
  }

  doMath(Option(1)).log
  doMath(List(1, 2, 3)).log

  final case class Box[A](value: A)
  implicit val boxFunctor: Functor[Box] = new Functor[Box] {
    override def map[A, B](fa: Box[A])(f: A => B): Box[B] = {
      Box(f(fa.value))
    }
  }

  val box = Box[Int](123)
  box.map(value => value + 1).log
  doMath(box).log

  import scala.concurrent.{Future, ExecutionContext}
  implicit def futureFunctor(implicit ec: ExecutionContext): Functor[Future] = new Functor[Future] {
    override def map[A, B](value: Future[A])(f: A => B): Future[B] = value.map(f)
  }
}
