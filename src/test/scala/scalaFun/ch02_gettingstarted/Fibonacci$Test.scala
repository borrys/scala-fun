package scalaFun.ch02_gettingstarted

import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class Fibonacci$Test extends FlatSpec with TableDrivenPropertyChecks {
  val fibonacciNumbers = Table(
    ("n", "fib"),
    (0, 0),
    (1, 1),
    (2, 1),
    (3, 2),
    (4, 3),
    (5, 5),
    (6, 8)
  )

  behavior of "Fibonacci"

  forAll (fibonacciNumbers) {(n: Int, fib: Int) => {
    it should s"return $fib for $n element of sequence" in {
      assert(Fibonacci.getNth(n) == fib)
    }
  }}

  it should "return sum of two previous elements" in {
    assert(Fibonacci.getNth(25) == Fibonacci.getNth(23) + Fibonacci.getNth(24))
  }
}

