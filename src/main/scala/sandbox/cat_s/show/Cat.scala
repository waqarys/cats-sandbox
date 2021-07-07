package sandbox.cat_s.show

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

case class Cat(name: String, age: Int, color: String)

object Cat{
  implicit val catShow: Show[Cat] = Show.show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }

  import cats.Eq
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.eq._

  implicit val catEqual: Eq[Cat] =
    Eq.instance[Cat]{ (cat1, cat2) =>
      (cat1.name === cat2.name) &&
        (cat1.age === cat2.age) &&
        (cat1.color === cat2.color)
    }
}

object Main extends App {
  println(Cat("Garfield", 38, "ginger and black").show)

  val cat1 = Cat("Gaarfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 32, "orange and black")

  import cats.syntax.eq._
  println(cat1 === cat2 )
  println(cat1 =!= cat2 )

  import cats.instances.option._
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]
  println(optionCat1 === optionCat2)
  println(optionCat1 =!= optionCat2)
}