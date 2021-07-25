package sandbox.monads.reader

/***
 * Reader Monad allows us to sequence operations that depends on some input.
 */
object ReaderExample extends App {

  import cats.data.Reader

  final case class Cat(name: String, favouriteFood: String)

  val catName: Reader[Cat, String] =
    Reader(cat => cat.name)

  import sandbox.Logger
  catName.run(Cat("Garfield", "lasagne")).log()

  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello $name")

  greetKitty.run(Cat("Heathcliff", "junk food")).log()

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favouriteFood}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed  <- feedKitty
    } yield s"$greet. $feed."

  greetAndFeed(Cat("Garfield", "lasagne")).log()
  greetAndFeed(Cat("Heathcliff", "junk food")).log()
}
