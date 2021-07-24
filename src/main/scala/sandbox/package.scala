import cats.data.Writer

package object sandbox {

  implicit class Logger[A](value: A){
    def log(str: String = "") = println(s"$str ${value.toString}")
  }

  implicit class WriterLog[W, A](writer: Writer[W, A]) {
    def logW() = println{
      s"Log: ${writer.written.toString}," +
        s"Value: ${writer.value.toString}"
    }
  }
}
