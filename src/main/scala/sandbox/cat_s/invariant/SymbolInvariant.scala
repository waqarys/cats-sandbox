package sandbox.cat_s.invariant

object SymbolInvariant extends App{

  import cats.Monoid
  import cats.instances.string._
  import cats.syntax.invariant._
  import cats.syntax.semigroup._

  implicit val symbolMonoid: Monoid[Symbol] =
    Monoid[String].imap(Symbol.apply)(_.name)


  implicit class Logger[A](value: A){
    def log()= println(value.toString)
  }
  Monoid[Symbol].empty.log()
  (Symbol("a") |+| Symbol("few") |+| Symbol("words")).log()
}
