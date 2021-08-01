package sandbox.monads.state

import cats.data.State

object Calculator extends App {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int]{ stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int]{
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)

      case _ =>
        sys.error("Fail!")
    }

  import sandbox.Logger
  evalOne("42").runA(Nil).value.log()

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  program.runA(Nil).value.log()

  import cats.syntax.applicative._

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]){ (a, b) =>
      a.flatMap(_ => evalOne(b))
    }

  val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))
  multistageProgram.runA(Nil).value.log()

  val biggerProgram = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  biggerProgram.runA(Nil).value.log()

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  evalInput("1 2 + 3 4 + *").log()
}
