package sandbox.typeclass.printable

case class Box[A](value: A)

object Box {
  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)

  implicit val boxIntPrintable: Printable[Box[Int]] = new Printable[Box[Int]] {
    override def format(box: Box[Int]): String = s"${box.value}"
  }
//  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
//    new Printable[Box[A]] {
//      override def format(box: Box[A]): String = p.format(box.value)
//    }
}
