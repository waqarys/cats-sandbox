package sandbox.functors.tree

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

import cats.Functor
import cats.implicits.toFunctorOps
object Tree {
  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
        tree match {
          case Branch(left, right) =>
            Branch(map(left)(f), map(right)(f))
          case Leaf(value) =>
            Leaf(f(value))
        }
    }

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)
}

object TreeMain extends App {
  implicit class Logger[A](value: A){
    def log()= println(value.toString)
  }

  Tree.leaf(100).map(_ * 2).log()
  Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2).log()
}