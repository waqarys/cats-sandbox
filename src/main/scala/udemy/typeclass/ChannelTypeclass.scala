package udemy.typeclass

import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  implicit object StringByteEncoder extends ByteEncoder[String] {
    override def encode(s: String): Array[Byte] = {
      s.getBytes
    }
  }
}

trait Channel {
  def write[A](obj: A)(implicit enc: ByteEncoder[A]) : Unit
}

object FileChannel extends Channel {
  override def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)

    Using(new FileOutputStream("test")){os =>
      os.write(bytes)
      os.flush()
    }
  }
}

object IntByteEncoder extends ByteEncoder[Int] {
  override def encode(n: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(n)
    bb.array()
  }
}

case class Switch(isOn: Boolean)
object Switch {
  implicit object SwitchByteEncoder extends ByteEncoder[Switch] {
    override def encode(a: Switch): Array[Byte] = {
      Array(a match {
        case Switch(true) => '1'.toByte
        case Switch(false) => '0'.toByte
      })
    }
  }
}

object ChannelTypeclass extends App {
  //FileChannel.write[Int](5, IntByteEncoder)
  FileChannel.write[String]("hello")
  FileChannel.write[Switch](Switch(false))
}