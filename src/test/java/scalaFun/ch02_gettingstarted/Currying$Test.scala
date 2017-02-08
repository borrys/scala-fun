package scalaFun.ch02_gettingstarted

import org.scalatest.{FlatSpec, FunSuite}

import scalaFun.ch02_gettingstarted.Currying.{curry, uncurry}

class Currying$Test extends FlatSpec {
  "Currying.curry" should "allow to execute uncurried function" in {
    val uncurried = (a:Int, b:Double) => (a,b).toString()

    val curried = curry(uncurried)

    assert(curried(1)(2.0) == uncurried(1, 2.0))
  }

  "Currying.uncurry" should "allow to execute curried function" in {
    val curried = (a: Double) => (b: Int) => (a, b).toString()

    val uncurried = uncurry(curried)

    assert(uncurried(1.0, 2) == curried(1.0)(2))
  }

  "Currying.curry and Currying.uncurry" should "form whole round trip" in {
    val uncurried = (a: Array[Int], b: Int) => b :: a.toList

    val uncurriedAgain = uncurry(curry(uncurried))

    val array = Array(1, 2, 3)
    val newValue = 4
    assert(uncurried(array, newValue) == uncurriedAgain(array, newValue))
  }

  "Currying.compose" should "compose two functions" in {
    val composed = Currying.compose((a: Int) => a.toString, (a: Double) => a.toInt)

    assert(composed(1.2) == "1")
  }
}
