package object sandbox {

  implicit class Logger[A](value: A){
    def log(str: String = "") = println(s"$str ${value.toString}")
  }
}
