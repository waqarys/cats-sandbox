package sandbox.typeclass.printable

trait Printable[A] {
  def format(value: A): String
}

object Printable {
  def format[A](input: A)(implicit p: Printable[A]): String = p.format(input)

  def print[A](input: A)(implicit p: Printable[A]): Unit = println(format(input))
}

object PrintableInstances {
  implicit val stringPrintable = new Printable[String] {
    override def format(input: String): String = input
  }

  implicit val intPrintable = new Printable[Int] {
    override def format(value: Int): String = value.toString
  }
}
