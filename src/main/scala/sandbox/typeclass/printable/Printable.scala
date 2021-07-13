package sandbox.typeclass.printable

trait Printable[A] { self =>
  def format(value: A): String

  //Functor
  def contramap[B](func: B => A): Printable[B] = new Printable[B] {
    override def format(value: B): String =
      self.format(func(value))
  }
}

object Printable {
  def format[A](input: A)(implicit p: Printable[A]): String = p.format(input)

  def print[A](input: A)(implicit p: Printable[A]): Unit = println(format(input))
}

object PrintableInstances {
  implicit val stringPrintable = new Printable[String] {
    override def format(value: String): String = s"'$value'"
  }

  implicit val intPrintable = new Printable[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit val booleanPrintable = new Printable[Boolean] {
    override def format(value: Boolean): String = if(value) "yes" else "no"
  }
}
