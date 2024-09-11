trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  implicit object StringByteEncoder extends ByteEncoder[String] {
    override def encode(s: String): Array[Byte] = s.getBytes
  }

  //def summon[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev
  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev
}

//Comment and uncomment this class to test implicit scopes
implicit object Rot3StringByteEncoder extends ByteEncoder[String] {
  override def encode(s: String): Array[Byte] =
    s.getBytes.map(b => (b + 3).toByte)
}

//1.
ByteEncoder.StringByteEncoder.encode("hello")

//2.
implicitly[ByteEncoder[String]].encode("hello")

//3.
//ByteEncoder.summon[String].encode("hello")
ByteEncoder.apply[String].encode("hello")
ByteEncoder[String].encode("hello")