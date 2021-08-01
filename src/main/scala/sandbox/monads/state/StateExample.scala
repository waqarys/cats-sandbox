package sandbox.monads.state

import cats.data.State

object StateExample extends App {
  val step1 = State[Int, String]{num =>
    val ans = num + 1
    (ans, s"Result of Step1: $ans")
  }

  val step2 = State[Int, String]{num =>
    val ans = num * 2
    (ans, s"Result of Step2: $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  import sandbox.Logger
  val (state, result) = both.run(20).value
  state.log()
  result.log()
}
