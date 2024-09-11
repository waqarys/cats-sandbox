import java.nio.ByteBuffer

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev
}

implicit object StringByteEncoder extends ByteEncoder[String] {
  override def encode(s: String): Array[Byte] = s.getBytes
}

implicit object IntByteEncoder extends ByteEncoder[Int] {
  override def encode(n: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(n)
    bb.array()
  }
}

//implicit object OptionString extends ByteEncoder[Option[String]] {
//
//  override def encode(s: Option[String]): Array[Byte] = s match {
//    case Some(value) => StringByteEncoder.encode(value)
//    case None => Array[Byte]()
//  }
//}
//
//implicit object OptionInt extends ByteEncoder[Option[Int]] {
//
//  override def encode(s: Option[Int]): Array[Byte] = s match {
//    case Some(value) => IntByteEncoder.encode(value)
//    case None => Array[Byte]()
//  }
//}

implicit def optionEncoder[A](implicit encA: ByteEncoder[A]): ByteEncoder[Option[A]] = new ByteEncoder[Option[A]] {
  override def encode(a: Option[A]) = {
    a match {
      case Some(value) => encA.encode(value)
      case None => Array[Byte]()
    }
  }
}

ByteEncoder[String].encode("hello")
ByteEncoder[Int].encode(1000)
ByteEncoder[Option[String]].encode(Option("world"))
ByteEncoder[Option[String]].encode(None)
ByteEncoder[Option[Int]].encode(Option(1000))
ByteEncoder[Option[Int]].encode(None)