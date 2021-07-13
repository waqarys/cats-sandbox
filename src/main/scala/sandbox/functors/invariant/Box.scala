package sandbox.functors.invariant

final case class Box[A](value: A)

object Box {
  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
    c.imap[Box[A]](Box(_), _.value)
}
