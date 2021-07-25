package sandbox.monads.reader

final case class Db(
                     usernames: Map[Int, String],
                     passwords: Map[String, String]
                   )



object Db extends App {

  import cats.data.Reader

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(
                   username: String,
                   password: String
                   ): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  import cats.syntax.applicative._  //for pure
  def checkLogin(
                userId: Int,
                password: String
                ): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      passwordOk <- username.map{ username =>
        checkPassword(username, password)
      }.getOrElse{
        false.pure[DbReader]
      }
    } yield passwordOk

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  import sandbox.Logger
  checkLogin(1, "zerocool").run(db).log()
  checkLogin(4, "davinci").run(db).log()
}
