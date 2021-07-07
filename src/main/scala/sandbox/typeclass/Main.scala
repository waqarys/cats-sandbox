package sandbox.typeclass

object Main extends App {

  import JsonWriterInstances._

  println(Json.toJson(Person("Waqar", "waqaryshaikh@gmail.com")))

  import JsonSyntax._
  println(Person("Waqar", "waqaryshaikh@gmail.com").toJson)
}
