package object sandbox {

  implicit class Logger[A](value: A){
    def log() = println(value.toString)
  }
}
