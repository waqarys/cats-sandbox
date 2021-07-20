package sandbox.monads.eval

import cats.Eval
import sandbox.Logger

object EvalApp extends App {

  val ans = for {
    a <- Eval.now{println("Calculating A"); 40}
    b <- Eval.always{ println("Calculating B"); 2 }
  } yield a + b
  ans.value.log()
  ans.value.log()

  //memoize method allows a chain of operations to be memoized.
  val saying = Eval
    .always{ println("Step 1"); "The cat" }
    .map{ str => println("Step 2"); s"$str sat on" }
    .memoize
    .map{ str => println("Step 3"); s"$str the mat" }
  saying.value.log()
  saying.value.log()

  //Trampoline - reusing stack
  def factorial(n: BigInt): Eval[BigInt] =
    if(n == 1) Eval.now(n)
    else Eval.defer(factorial(n - 1).map(_ * n))

  factorial(50).value.log("factorial")
}
