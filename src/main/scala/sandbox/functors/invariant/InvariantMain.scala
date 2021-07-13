package sandbox.functors.invariant

object InvariantMain extends App {

  implicit class Logger[A](value: A){
    def log()= println(value.toString)
  }

  Codec.encode(123.4).log()
  Codec.decode[Double]("123.4").log()

  Codec.encode(Box(123.4)).log()
  Codec.decode[Box[Double]]("123.4").log()
}
