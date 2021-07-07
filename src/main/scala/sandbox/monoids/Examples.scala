package sandbox.monoids

object Examples extends App {
  import cats.Monoid
  import cats.instances.int._ //for Monoid
  import cats.syntax.semigroup._  //for |+|
  import cats.instances.option._

//  def addAll(items: List[Int]): Int =
//    items.foldLeft(Monoid[Int].empty)(_ |+| _)

  def add[A](items: List[A])(implicit monoid: Monoid[A]):A =
    items.foldLeft(monoid.empty)(_ |+| _)

  println(add(List(1,2,3)))
  println(add(List(Some(1), Some(2), None,Some(3))))

  case class Order(totalCost: Double, quantity: Double)
  object Order{
    implicit val monoid: Monoid[Order] = new Monoid[Order]{
      override def combine(x: Order, y: Order): Order = {
        Order(
          x.totalCost + y.totalCost,
          x.quantity + y.quantity
        )
      }

      override def empty: Order = Order(0, 0)
    }
  }
}
