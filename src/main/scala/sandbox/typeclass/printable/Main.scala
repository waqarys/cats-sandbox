package sandbox.typeclass.printable

object Main extends App {
  val cat = Cat("Garfield", 4, "ginger and white")
  Printable.print(cat)

  import PrintableSyntax._
  Cat("Garfield", 4, "ginger and white").print

  Box(123).print
}
