package casestudies.typeconstructorsandmonads

import cats.Applicative
import cats.syntax.functor._
import cats.instances.list._
import cats.syntax.traverse._

//class UptimeService[F[_]](client: UptimeClient[F])(implicit a: Applicative[F]) {
//  def getTotalUptime(hostnames: List[String]): F[Int] =
//    hostnames.traverse(client.getUptime).map(_.sum)
//}

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}