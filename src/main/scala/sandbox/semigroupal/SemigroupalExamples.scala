package sandbox.semigroupal

/**
 * Semigroup    -> allows to join values
 * Semogroupal  -> allows to join Contexts
 */
object SemigroupalExamples extends App {

  import cats.Semigroupal
  import cats.instances.option._

  import sandbox.Logger
  Semigroupal[Option].product(Some(123), Some("abc")).log()
  Semigroupal[Option].product(None, Some("abc")).log()

  Semigroupal.tuple3(Option(1), Option(2), Option(3)).log()
  Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int]).log()

  Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _).log()
  Semigroupal.map2(Option(1), Option.empty[Int])(_ + _).log()

  import cats.syntax.apply._  //for tupled and mapN
  final case class Cat(name: String, born: Int, color: String)

  (
    Option("Garfield"),
    Option(1978),
    Option("Orange & black")
  ).mapN(Cat.apply).log()
}
